module Shinobu.Effects.Cache where

import qualified Database.SQLite.Simple as SQL
import qualified Polysemy as P
import qualified Polysemy.Conc as P
import qualified Polysemy.Conc.Sync as P
import qualified Polysemy.Resource as P
import qualified Shinobu.Effects.DB as DB

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

runCacheSync ::
  forall a v r.
  [P.Sync v, P.Mask, P.Resource] :>> r =>
  P.Sem r v ->
  P.Sem (Cache (P.Sem r) v : r) a ->
  P.Sem r a
runCacheSync reloadIO sem = do
  P.putBlock =<< reloadIO
  sem & P.interpret \case
    Get -> P.block
    Modify f -> P.modify $
      const $
        P.mask do
          a <- P.raise reloadIO
          r <- P.restore (P.raise (f a)) `P.onException` P.raise reloadIO
          a' <- P.raise reloadIO
          return (a', r)
    Put m f -> P.modify $
      const $
        P.mask do
          P.restore (P.raise m) `P.onException` P.raise reloadIO
          a <- P.raise reloadIO
          return (a, f a)
    Reload -> P.modify_ $ const reloadIO

runCacheDB ::
  forall a v r.
  [DB.SQLite, P.Sync v, P.Mask, P.Resource] :>> r =>
  (SQL.Connection -> IO v) ->
  P.Sem (Cache (P.Sem r) v : r) a ->
  P.Sem r a
runCacheDB = runCacheSync . DB.run
