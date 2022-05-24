module Shinobu.Commands.CallNotification where

import Calamity
import Control.Error (justZ)
import qualified Data.Map.Strict as M
import qualified Polysemy as P
import qualified Polysemy.NonDet as P
import qualified Polysemy.Tagged as P
import Shinobu.Effects.Cooldown
import Shinobu.Effects.KeyStore
import Shinobu.Types
import Shinobu.Util (tellInfo)

voiceChannelMembers :: BotC r => VoiceChannel -> P.Sem r (Maybe [Member])
voiceChannelMembers voiceChannel = P.runNonDetMaybe do
  let justEmpty = maybe empty pure
      guildID = view #guildID voiceChannel
      memberFromUID = upgrade . (guildID,) . coerceSnowflake
      memberFromVoiceState = justEmpty <=< memberFromUID . view #userID
  guild <- justEmpty =<< upgrade guildID
  mapM memberFromVoiceState (view #voiceStates guild)

runVcToTcInIO sem = do
  vcToTcMap <-
    newIORef $
      M.fromList $
        map
          (bimap Snowflake Snowflake)
          []
  runKeyStoreIORef vcToTcMap . P.untag @VCTOTC $ sem

data VCTOTC

type VcToTc = P.Tagged VCTOTC (KeyStore (Snowflake VoiceChannel) (Snowflake TextChannel))

vcToTc :: VcToTc :> r => Snowflake VoiceChannel -> P.Sem r (Maybe (Snowflake TextChannel))
vcToTc = P.tag @VCTOTC . getK

callReaction :: VcToTc :> r => ShinobuSem r
callReaction = void $
  react @'VoiceStateUpdateEvt
    \(mBefore, after) -> void $ P.runNonDetMaybe do
      assertReady

      afterID <- justZ (after ^. #channelID)

      -- make sure the user switched channels (or joined one)
      whenJust mBefore \before -> do
        whenJust (before ^. #channelID) \beforeID -> do
          guard (beforeID /= afterID)

      -- make sure the person joined an empty voice channel
      vc <- justZ =<< upgrade afterID
      vcMembers <- justZ =<< voiceChannelMembers vc
      guard (length vcMembers == 1)

      -- send the reaction
      textChannel <- justZ =<< vcToTc afterID
      user <- justZ =<< upgrade (after ^. #userID)
      tellInfo textChannel [i|#{mention user} has started a call.|]

      setCooldown 5
