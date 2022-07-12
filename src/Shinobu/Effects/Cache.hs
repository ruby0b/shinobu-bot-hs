module Shinobu.Effects.Cache where

import Control.Concurrent (modifyMVar_)
import qualified Database.SQLite.Simple as SQL
import qualified Polysemy as P

data Cache v :: P.Effect where
  Get :: Cache v m v
  Reload :: Cache v m ()

P.makeSem ''Cache

runCacheIO :: P.Embed IO :> r => IO v -> P.Sem (Cache v : r) b -> P.Sem r b
runCacheIO reloadIO sem = do
  -- TODO: look into strict MVar
  mvar <- P.embed $ newMVar =<< reloadIO
  sem & P.interpret \case
    Get -> P.embed $ readMVar mvar
    Reload -> P.embed $ modifyMVar_ mvar $ const reloadIO

runCacheDB :: P.Embed IO :> r => (SQL.Connection -> IO v) -> P.Sem (Cache v : r) b -> P.Sem r b
runCacheDB fetchSQL = do
  runCacheIO (SQL.open "shinobu.db" >>= fetchSQL)


