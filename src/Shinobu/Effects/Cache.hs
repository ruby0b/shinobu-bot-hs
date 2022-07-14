module Shinobu.Effects.Cache where

import Control.Concurrent (modifyMVar, modifyMVar_)
import Control.Exception (mask, onException)
import qualified Database.SQLite.Simple as SQL
import qualified Polysemy as P

data Cache m v :: P.Effect where
  -- | get the cached value
  Get :: Cache m v n v
  -- | reloads cache before and after running an effectful function
  Modify :: (v -> m a) -> Cache m v n a
  -- | modifies the resource using a side effect, then reloads the cache and retrieves something from it
  Put :: m a -> (v -> a) -> Cache m v n a
  -- | reload the cache (consider using modify if you're using 'get' right afterwards)
  Reload :: Cache m v n ()

P.makeSem ''Cache

runCacheMVar :: (P.Embed IO :> r) => IO v -> P.Sem (Cache IO v : r) b -> P.Sem r b
runCacheMVar reloadIO sem = do
  mvar <- P.embed $ newMVar =<< reloadIO
  sem & P.interpret \case
    Get -> P.embed $ readMVar mvar
    Modify f -> P.embed $
      modifyMVar mvar $
        const $
          mask $ \restore -> do
            a <- reloadIO
            r <- restore (f a) `onException` reloadIO
            a' <- reloadIO
            return (a', r)
    Put m f -> P.embed $
      modifyMVar mvar $
        const $
          mask $ \restore -> do
            restore m `onException` reloadIO
            a <- reloadIO
            return (a, f a)
    Reload -> P.embed $ modifyMVar_ mvar $ const reloadIO

runCacheDB :: forall v r b. (P.Embed IO :> r) => (SQL.Connection -> IO v) -> P.Sem (Cache IO v : r) b -> P.Sem r b
runCacheDB fetchSQL = do
  runCacheMVar (SQL.open "shinobu.db" >>= fetchSQL)
