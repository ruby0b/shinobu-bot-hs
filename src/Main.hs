module Main where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context
import Calamity.Metrics.Noop
import Data.Flags
import Data.MonoTraversable (MonoFoldable (onull))
import Data.Text (strip)
import qualified Data.Text.IO as T
import qualified Di
import qualified DiPolysemy as P
import qualified Polysemy as P
import qualified Polysemy.Conc as P
import qualified Polysemy.RandomFu as P
import qualified Polysemy.Resource as P
import Shinobu.Commands.BannedPatterns
import Shinobu.Commands.CallNotification
import Shinobu.Commands.CustomReactions
import Shinobu.Commands.ErrorHandling
import Shinobu.Commands.Misc
import Shinobu.Commands.Shop
import Shinobu.Effects.Cooldown
import qualified Shinobu.Effects.DB as DB
import Shinobu.Gacha.DB
import Shinobu.Util

main :: IO ()
main = do
  token <- strip <$> T.readFile "./TOKEN"
  when (onull token) $
    die "Error: Put your bot token into ./TOKEN"
  Di.new \di -> void
    . P.runFinal
    . P.embedToFinal
    . P.runDiToIO di
    . P.runRandomIO
    . P.resourceToIOFinal
    . P.interpretMaskFinal
    . DB.runSqliteSimple "shinobu.db"
    . runCooldownInIO
    . runGachaStoresIO
    . handleFailByLogging
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
