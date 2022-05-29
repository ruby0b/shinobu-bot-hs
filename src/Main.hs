module Main where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context
import Calamity.Metrics.Noop
import Data.Flags
import Data.MonoTraversable (MonoFoldable (onull))
import Data.Text (strip)
import qualified Di
import qualified DiPolysemy as P
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Fail as P
import qualified Polysemy.RandomFu as P
import Shinobu.Commands.BannedPatterns
import Shinobu.Commands.CallNotification
import Shinobu.Commands.CustomReactions
import Shinobu.Commands.ErrorHandling
import Shinobu.Commands.Misc
import Shinobu.Commands.Shop
import Shinobu.Effects.Cooldown
import Shinobu.Gacha.DB
import Shinobu.Util

stringErrorToFail :: P.Fail :> r => P.Sem (P.Error String : r) a -> P.Sem r a
stringErrorToFail err =
  P.runError err >>= \case
    Left e -> fail e
    Right v -> pure v

main :: IO ()
main = do
  token <- strip <$> readFileText "./TOKEN"
  when (onull token) $
    fail "Put your bot token into ./TOKEN"
  Di.new \di -> void
    . P.runFinal
    . P.embedToFinal
    . P.runDiToIO di
    . P.runRandomIO
    . runCooldownInIO
    . runVcToTcInIO
    . runGachaStoresIO
    . handleFailByLogging
    . stringErrorToFail
    . runCacheInMemory
    . runMetricsNoop
    . useConstantPrefix "="
    . useFullContext
    . runBotIO (BotToken token) allFlags
    $ addCommands do
      helpCommand
      miscCommands
      packCmd
      customReactions
      callReaction
      tellErrors
      bannedPatterns
