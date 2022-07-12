module Shinobu.Commands.CallNotification where

import Calamity
import Calamity.Commands
import Control.Error (fmapL, justZ)
import qualified Data.Map.Strict as M
import Database.SQLite.Simple.QQ.Interpolated
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.NonDet as P
import Shinobu.DB ()
import Shinobu.Effects.Cooldown
import qualified Shinobu.Effects.KeyStore as Id
import Shinobu.Effects.UserError
import Shinobu.KeyStoreCommands
import Shinobu.Types
import Shinobu.Util

voiceChannelMembers :: (BotC r, P.Error String :> r) => VoiceChannel -> P.Sem r [Member]
voiceChannelMembers voiceChannel = do
  let guildID = view #guildID voiceChannel
      memberFromUID = upgrade . (guildID,) . coerceSnowflake
      memberFromVoiceState = P.note "Failed to get Member" <=< memberFromUID . view #userID
  -- TODO: cache this somehow
  res <- invoke $ GetGuild guildID
  print res
  guild <- P.fromEither (fmapL show res)
  mapM memberFromVoiceState (guild ^. #voiceStates)

-- TODO optimization: use Map Id (Map VC TC) instead of Map Id [(VC, TC)]
callReaction :: ShinobuSem r
callReaction = void
  . Id.runKeyStoreCachedDB
    (M.fromList . map (\(id_, vc, tc) -> (id_, (vc, tc))) <.> [iquery|SELECT * FROM voice_to_text|])
    (\id_ (vc, tc) -> [iexecute|INSERT INTO voice_to_text VALUES (${id_}, ${vc}, ${tc})|])
    (\id_ -> [iexecute|DELETE FROM voice_to_text WHERE id=${id_}|])
    [iexecute|DELETE FROM voice_to_text|]
  $ do
    react @'VoiceStateUpdateEvt
      \(mBefore, after) -> void $ P.runNonDetMaybe do
        print mBefore
        print after
        assertReady
        print "Ready"

        afterID <- justZ (after ^. #channelID)

        -- make sure the user switched channels (or joined one)
        whenJust mBefore \before -> do
          whenJust (before ^. #channelID) \beforeID -> do
            guard (beforeID /= afterID)
        print "Switched channels"

        -- make sure the person joined an empty voice channel
        vc <- justZ =<< upgrade afterID
        print "Upgraded channel"
        vcMembers <- voiceChannelMembers vc
        print vcMembers
        guard (length vcMembers == 1)

        -- send the reaction
        print "Getting mapped VC..."
        vcToTc <- map snd <$> Id.listKeyValuePairs
        (_, textChannel) <- justZ . find ((== afterID) . fst) $ vcToTc
        print "Success!"
        user <- justZ =<< upgrade (after ^. #userID)
        tellInfo textChannel [i|#{mention user} started a call.|]

        setCooldown 5

    let spec = KeyStoreSpec {groupName = "ring", itemSingular = "call notification", itemPlural = "call notifications"}

    help (const [i|Manage #{spec ^. #itemPlural}|])
      . requires' "Admin" isAdminCtx
      . group (spec ^. #groupName)
      $ runUserErrorTellEmbed do
        help (const [i|Add a new #{spec ^. #itemSingular}|])
          . command @'[Named "Voice Channel ID" (Snowflake VoiceChannel), Named "Text Channel ID" (Snowflake TextChannel)] "add"
          $ \ctx vcId tcId -> void do
            -- TODO replace this with actual checks/upgrades
            pure (Just vcId) >>= maybeUserError ctx [i|Invalid Voice Channel: #{vcId}|]
            pure (Nothing) >>= maybeUserError ctx [i|Invalid Text Channel: #{tcId}|]
            Id.insertNewKey (vcId, tcId)
            tellSuccess ctx [i|Understood!\nI will notify the channel #{mention tcId} whenever a call is started in #{mention vcId}|]

        mkListCommand spec \id_ (vc, tc) ->
          [i|#{id_}: #{mention vc} 📢 #{mention tc}|]

        mkDeleteCommand @Integer spec \_id (vc, tc) ->
          [i|#{mention vc} 📢 #{mention tc}|]

        mkReloadCommand spec
