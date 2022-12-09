{-# OPTIONS_GHC -Wno-orphans #-}

module Shinobu.Commands.CallNotification where

import Calamity
import Calamity.Cache.Eff
import Calamity.Commands
import Control.Error (justZ)
import qualified Data.Map.Strict as M
import Database.SQLite.Simple.QQ.Interpolated
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.NonDet as P
import qualified Shinobu.Effects.Cache as C
import Shinobu.Effects.Cooldown
import qualified Shinobu.Effects.KeyStore as Id
import Shinobu.Utils.Checks
import Shinobu.Utils.DB ()
import Shinobu.Utils.Error
import Shinobu.Utils.KeyStoreCommands
import Shinobu.Utils.Misc
import Shinobu.Utils.Types

voiceChannelMembers :: (BotC r, DiscordError :> r) => VoiceChannel -> P.Sem r [Member]
voiceChannelMembers voiceChannel = do
  let guildID = view #guildID voiceChannel
      memberFromUID = upgrade . (guildID,) . coerceSnowflake
      memberFromVoiceState = P.note "Failed to get Member" <=< memberFromUID . view #userID
  res <- getGuild guildID
  guild <- P.note "Failed to get Guild" res
  p $ guild ^. #voiceStates
  mapM memberFromVoiceState (guild ^. #voiceStates)

type VcToTc = Map (Snowflake VoiceChannel) (NonEmpty (Integer, Snowflake TextChannel))

instance Id.KeyStoreC VcToTc Integer (Snowflake VoiceChannel, Snowflake TextChannel) where
  toKVList = concatMap (\(vc, xs) -> toList $ fmap (\(id_, tc) -> (id_, (vc, tc))) xs) . M.toList
  lookupK id_ = snd <.> find ((== id_) . fst) . Id.toKVList

callReaction :: ShinobuSem r
callReaction = void
  . runSyncInIO
  . Id.runKeyStoreAsDBCache @VcToTc @Integer @(Snowflake VoiceChannel, Snowflake TextChannel)
    (M.fromAscList . indexByFst . map (\(x, y, z) -> (y, x, z)) <.> [iquery|SELECT * FROM voice_to_text|])
    (\id_ (vc, tc) -> [iexecute|INSERT OR REPLACE INTO voice_to_text VALUES (${id_}, ${vc}, ${tc})|])
    (\id_ -> [iexecute|DELETE FROM voice_to_text WHERE id=${id_}|])
    [iexecute|DELETE FROM voice_to_text|]
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
          whenJust mBefore \before -> do
            whenJust (before ^. #channelID) \beforeID -> do
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
          textChannels <- justZ . M.lookup afterID =<< C.get
          p' "Success!"
          user <- justZ =<< upgrade (after ^. #userID)
          for_ textChannels \(_, tc) ->
            tellInfo tc [i|#{mention user} started a call in #{mention afterID}.|]

          setCooldown 5

    let spec = KeyStoreSpec {groupName = "ring", itemSingular = "call notification", itemPlural = "call notifications"}

    help_ [i|Manage #{spec ^. #itemPlural}|]
      . requiresAdmin
      . group (spec ^. #groupName)
      $ do
        help_ [i|Add a new #{spec ^. #itemSingular}|]
          . command @'[Named "Voice Channel ID" (Snowflake VoiceChannel), Named "Text Channel ID" (Snowflake TextChannel)] "add"
          $ \ctx vcId tcId -> tellMyErrors ctx do
            upgrade vcId >>= maybeThrow [i|Invalid Voice Channel: #{vcId}|]
            upgrade tcId >>= maybeThrow [i|Invalid Text Channel: #{tcId}|]
            Id.insertNewKey (vcId, tcId)
            tellSuccess ctx [i|Understood!\nI will notify the channel #{mention tcId} whenever a call is started in #{mention vcId}|]

        mkListCommand spec \id_ (vc, tc) ->
          [i|#{id_}: #{mention vc} 📢 #{mention tc}|]

        mkDeleteCommand @Integer spec \_id (vc, tc) ->
          [i|#{mention vc} 📢 #{mention tc}|]

        mkReloadCommand spec
