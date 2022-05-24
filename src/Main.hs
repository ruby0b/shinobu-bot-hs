module Main where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context
import Calamity.Metrics.Noop
import Data.Flags
import qualified Di
import qualified DiPolysemy as P
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Fail as P
import qualified Polysemy.RandomFu as P
import Shinobu.Commands.BannedPatterns (bannedPatterns)
import Shinobu.Commands.CallNotification
import Shinobu.Commands.CustomReactions
import Shinobu.Commands.ErrorHandling (tellErrors)
import Shinobu.Commands.Misc
import Shinobu.Commands.Shop
import Shinobu.Effects.Cooldown (runCooldownInIO)
import Shinobu.Gacha.DB (runGachaStoresIO)
import Shinobu.Util

getToken :: MonadIO f => f Text
getToken =
  let p = "./TOKEN"
   in (// error "Put your bot token in " <> p)
        . lines
        <$> readFileText p

stringErrorToFail :: P.Fail :> r => P.Sem (P.Error String : r) a -> P.Sem r a
stringErrorToFail err =
  P.runError err >>= \case
    Left e -> fail e
    Right v -> pure v

main :: IO ()
main = do
  token <- getToken
  Di.new $ \di -> void
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
    $ runBotIO (BotToken token) allFlags do
      addCommands do
        helpCommand
        miscCommands
        packCmd
        customReactions
        callReaction
        tellErrors
        bannedPatterns
