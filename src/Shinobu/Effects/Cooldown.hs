module Shinobu.Effects.Cooldown where

import Data.Fixed (Pico)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import qualified Polysemy as P
import qualified Polysemy.NonDet as P
import qualified Polysemy.State as P

data Ready = Ready | TimeLeft Pico
  deriving (Show)

data Cooldown :: P.Effect where
  GetCooldown :: Cooldown m Ready
  SetCooldown :: NominalDiffTime -> Cooldown m ()

P.makeSem ''Cooldown

-- | Run f on the time left or do nothing if we're ready.
whenNotReady :: Cooldown :> r => (Pico -> P.Sem r ()) -> P.Sem r ()
whenNotReady f =
  getCooldown >>= \case
    Ready -> return ()
    TimeLeft x -> f x

-- | 'empty' if we're not  ready yet; do nothing if we are.
assertReady :: [Cooldown, P.NonDet] :>> r => P.Sem r ()
assertReady = whenNotReady $ const mzero

runCooldown ::
  P.Embed IO :> r =>
  P.Sem (Cooldown : r) a ->
  P.Sem (P.State UTCTime : r) a
runCooldown = P.reinterpret \case
  GetCooldown -> do
    future <- P.get
    now <- P.embed getCurrentTime
    let diff = nominalDiffTimeToSeconds $ diffUTCTime future now
    return
      if diff > 0
        then TimeLeft diff
        else Ready
  SetCooldown newCooldown -> do
    now <- P.embed getCurrentTime
    let newReadyTime = addUTCTime newCooldown now
    P.put newReadyTime

runCooldownInIO ::
  P.Embed IO :> r =>
  P.Sem (Cooldown : r) a ->
  P.Sem r a
runCooldownInIO m = do
  now <- P.embed getCurrentTime
  var <- P.embed $ newIORef now
  P.runStateIORef var $ runCooldown m
