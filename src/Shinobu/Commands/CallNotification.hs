module Shinobu.Commands.CallNotification where

import Calamity
import Control.Error (fmapL, justZ)
import qualified Data.Map.Strict as M
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.NonDet as P
import Shinobu.DB ()
import Shinobu.Effects.Cooldown
import Shinobu.Effects.KeyStore
import qualified Shinobu.Effects.KeyStore as Id
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

callReaction :: ShinobuSem r
callReaction = void
  . ( \sem -> do
        conn <- P.embed $ SQL.open "shinobu.db"
        vals :: [(Snowflake VoiceChannel, Snowflake TextChannel)] <-
          P.embed $
            conn
              & [iquery|SELECT * FROM voice_to_text|]
        vcToTcMap <- newIORef $ M.fromList vals
        runKeyStoreIORef vcToTcMap sem
    )
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
        textChannel <- justZ =<< Id.lookup afterID
        print "Success!"
        user <- justZ =<< upgrade (after ^. #userID)
        tellInfo textChannel [i|#{mention user} started a call.|]

        setCooldown 5
