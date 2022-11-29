{-# OPTIONS_GHC -Wno-orphans #-}

module Shinobu.Commands.CallNotification where

import Calamity
import Calamity.Cache.Eff
import Calamity.Commands
import Control.Error (justZ)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.NonDet as P
import qualified Shinobu.Effects.CachedState as CS
import Shinobu.Effects.Cooldown
import Shinobu.Effects.UserError
import Shinobu.Utils.Checks
import Shinobu.Utils.DB ()
import Shinobu.Utils.JSON
import Shinobu.Utils.KeyStoreCommands
import Shinobu.Utils.Misc
import Shinobu.Utils.Types

data CallNotificationJSON = CallNotificationJSON
  { voiceChannel :: Word64,
    textChannel :: Word64
  }
  deriving (Generic, Show)

makeJSON ''CallNotificationJSON

data CallNotification = CallNotification
  { vc :: Snowflake VoiceChannel,
    tc :: Snowflake TextChannel
  }
  deriving (Generic)

instance TryFrom [CallNotificationJSON] [CallNotification] where
  tryFrom = pure . map \j -> CallNotification (Snowflake (j ^. #voiceChannel)) (Snowflake (j ^. #textChannel))

instance From [CallNotification] [CallNotificationJSON] where
  from = map \n -> CallNotificationJSON (fromSnowflake (n ^. #vc)) (fromSnowflake (n ^. #tc))

voiceChannelMembers :: (BotC r, P.Error String :> r) => VoiceChannel -> P.Sem r [Member]
voiceChannelMembers voiceChannel = do
  let guildID = view #guildID voiceChannel
      memberFromUID = upgrade . (guildID,) . coerceSnowflake
      memberFromVoiceState = P.note "Failed to get Member" <=< memberFromUID . view #userID
  res <- getGuild guildID
  guild <- P.note "Failed to get Guild" res
  p $ guild ^. #voiceStates
  mapM memberFromVoiceState (guild ^. #voiceStates)

callReaction :: ShinobuSem r
callReaction = void
  . runSyncInIO
  . CS.runCachedStateAsJsonFileVia' @[CallNotificationJSON] "data/call-notifications.json"
  $ do
    react @'VoiceStateUpdateEvt
      \(mBefore, after) -> void $
        P.runNonDetMaybe $ stringErrorToFail do
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
          textChannels <- guarded (not . null) . map (view #tc) . filter (\n -> n ^. #vc == afterID) =<< CS.cached @[CallNotification]
          p' "Success!"
          user <- justZ =<< upgrade (after ^. #userID)
          for_ textChannels \vc_ ->
            tellInfo vc_ [i|#{mention user} started a call in #{mention afterID}.|]

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
            CS.modify_ (CallNotification vcId tcId :)
            tellSuccess ctx [i|Understood!\nI will notify the channel #{mention tcId} whenever a call is started in #{mention vcId}|]

        mkListCommand spec \id_ CallNotification {..} ->
          [i|#{id_}: #{mention vc} 📢 #{mention tc}|]

        mkDeleteCommand spec \_id CallNotification {..} ->
          [i|#{mention vc} 📢 #{mention tc}|]

        mkReloadCommand spec
