module Shinobu.Commands.CallNotification where

import Calamity
import Calamity.Cache.Eff
import Calamity.Commands
import Control.Error (justZ)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.NonDet as P
import Shinobu.Effects.Cache
import Shinobu.Effects.Cooldown
import Shinobu.Effects.DB
import Shinobu.Utils.Checks
import Shinobu.Utils.DB ()
import Shinobu.Utils.Error
import Shinobu.Utils.KeyStoreCommands
import Shinobu.Utils.Misc
import Shinobu.Utils.Types

table = "voice_to_text"

spec =
  KeyStoreSpec
    { tableName = table,
      groupName = "ring",
      itemSingular = "call notification",
      itemPlural = "call notifications"
    }

voiceChannelMembers :: (BotC r, DiscordError :> r) => VoiceChannel -> P.Sem r [Member]
voiceChannelMembers voiceChannel = do
  let guildID = view #guildID voiceChannel
      memberFromUID = upgrade . (guildID,) . coerceSnowflake
      memberFromVoiceState voiceState = memberFromUID (voiceState ^. #userID) >>= P.note "Failed to get Member"
      correctVoiceChannel voiceState = isJust do
        channelID <- voiceState ^. #channelID
        guard (channelID == voiceChannel ^. #id)
  res <- getGuild guildID
  guild <- P.note "Failed to get Guild" res
  p $ guild ^. #voiceStates
  let voiceStatesInChannel = filter correctVoiceChannel (guild ^. #voiceStates)
  mapM memberFromVoiceState voiceStatesInChannel

type VcToTc = (Integer, Snowflake VoiceChannel, Snowflake TextChannel)

callReaction :: ShinobuSem r
callReaction = void
  . runSyncInIO
  . evalCacheViaState @[VcToTc] (query [isql|SELECT * FROM !{table}|])
  $ do
    react @'VoiceStateUpdateEvt
      \(mBefore, after) -> void
        . handleExceptionByLogging @DiscordErr
        . P.runNonDetMaybe
        $ do
          p mBefore
          p after
          assertReady
          p' "Ready"

          afterID <- justZ (after ^. #channelID)

          -- make sure the user switched channels (or joined one)
          whenJust (mBefore >>= view #channelID) \beforeID ->
            guard (beforeID /= afterID)
          p' "Switched channels"

          -- make sure the person joined an empty voice channel
          vc <- justZ =<< upgrade afterID
          p' "Upgraded channel"
          vcMembers <- voiceChannelMembers vc
          p vcMembers
          guard (length vcMembers == 1)

          -- send the reaction
          p' "Getting mapped VC..."
          config <- cached
          let textChannels = [t | (_, v, t) <- config, v == afterID]
          p' [i|Success! (found #{length textChannels} entries)|]
          user <- justZ =<< upgrade (after ^. #userID)
          for_ textChannels \tc ->
            tellInfo tc [i|#{mention user} started a call in #{mention afterID}.|]

          setCooldown 5

    help_ [i|Manage #{spec ^. #itemPlural}|]
      . requiresAdmin
      . group (spec ^. #groupName)
      $ do
        help_ [i|Add a new #{spec ^. #itemSingular}|]
          . command @'[Named "Voice Channel ID" (Snowflake VoiceChannel), Named "Text Channel ID" (Snowflake TextChannel)] "add"
          $ \ctx vcId tcId -> tellMyErrors ctx do
            upgrade vcId >>?! P.throw [i|Invalid Voice Channel: #{vcId}|]
            upgrade tcId >>?! P.throw [i|Invalid Text Channel: #{tcId}|]
            execute [isql|INSERT INTO !{table} (voice_id, text_id) VALUES ({vcId}, {tcId})|]
            refresh
            tellSuccess ctx [i|Understood!\nI will notify the channel #{mention tcId} whenever a call is started in #{mention vcId}|]

        mkListCommand spec \(id_, vc, tc) ->
          [i|#{id_}: #{mention vc} 📢 #{mention tc}|]

        mkDeleteCommand @Integer @VcToTc spec \(_id, vc, tc) ->
          [i|#{mention vc} 📢 #{mention tc}|]

        mkReloadCommand spec
