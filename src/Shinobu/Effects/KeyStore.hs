module Shinobu.Effects.KeyStore where

import Calamity
import Control.Concurrent (modifyMVar, modifyMVar_)
import qualified Data.Map.Strict as M
import qualified Database.SQLite.Simple as SQL
import qualified Polysemy as P
import qualified Polysemy.State as P
import Shinobu.Util (maximumOr, whenNothingRun)

class NewUnique a where
  -- | Returns a value that's not already in @f a@.
  newUnique :: (Foldable f, Functor f) => f a -> a

instance NewUnique Integer where
  newUnique = succ . maximumOr 0

instance NewUnique Word64 where
  newUnique = succ . maximumOr 0

instance NewUnique (Snowflake a) where
  newUnique = Snowflake . newUnique . fmap fromSnowflake

-- | A key value store
data KeyStore k v :: P.Effect where
  ListKeyValuePairs :: KeyStore k v m [(k, v)]
  Lookup :: k -> KeyStore k v m (Maybe v)
  -- | returns existing value that's under the same key on conflict
  InsertOrLookup :: k -> v -> KeyStore k v m (Maybe v)
  InsertOrReplace :: k -> v -> KeyStore k v m ()
  -- | returns the new auto-generated key
  InsertNewKey :: NewUnique k => v -> KeyStore k v m k
  Delete :: k -> KeyStore k v m (Maybe v)
  Clear :: KeyStore k v m ()
  Reload :: KeyStore k v m ()

P.makeSem ''KeyStore

runKeyStoreAsState :: forall k v r a. Ord k => P.Sem (KeyStore k v : r) a -> P.Sem (P.State (M.Map k v) : r) a
runKeyStoreAsState = P.reinterpret \case
  ListKeyValuePairs -> M.toList <$> P.get
  Lookup k -> M.lookup k <$> P.get
  InsertOrLookup k v -> do
    existing <- M.lookup k <$> P.get
    whenNothingRun existing $
      P.modify (M.insert k v)
  InsertOrReplace k v -> P.modify $ M.insert k v
  InsertNewKey v -> do
    list <- M.toList <$> P.get @(M.Map k v)
    let k = newUnique . map fst $ list
    P.modify (M.insert k v)
    return k
  Delete k -> do
    val <- M.lookup k <$> P.get
    P.modify (M.delete k)
    return val
  Clear -> P.put M.empty
  Reload -> return ()

runKeyStorePurely :: (Ord k) => M.Map k v -> P.Sem (KeyStore k v : r) a -> P.Sem r (M.Map k v, a)
runKeyStorePurely kvMap = P.runState kvMap . runKeyStoreAsState

runKeyStoreIORef :: (Ord k, P.Embed IO :> r) => IORef (M.Map k v) -> P.Sem (KeyStore k v : r) a -> P.Sem r a
runKeyStoreIORef ref = P.runStateIORef ref . runKeyStoreAsState

-- | Doesn't require IO for reads to improve read speeds.
-- Uses an MVar to cache IO results and subsequent updates.
-- 'reload' reads back from IO, overwriting the MVar.
-- /Assumption: The underlying IO structure is not modified externally./
runKeyStoreCachedIO ::
  forall k v r a.
  (P.Embed IO :> r, Ord k) =>
  (IO (Map k v), k -> v -> IO (), k -> IO (), IO ()) ->
  P.Sem (KeyStore k v : r) a ->
  P.Sem r a
runKeyStoreCachedIO (readIO, putIO, deleteIO, clearIO) sem = do
  -- TODO: look into strict MVar
  mvar <- P.embed $ newMVar =<< readIO
  let readMap :: P.Sem r (M.Map k v)
      readMap = P.embed $ readMVar mvar
      modifyMap :: forall x. (Map k v -> IO (Map k v, x)) -> P.Sem r x
      modifyMap = P.embed . modifyMVar mvar
      modifyMap_ :: (Map k v -> IO (Map k v)) -> P.Sem r ()
      modifyMap_ = P.embed . modifyMVar_ mvar

  sem & P.interpret \case
    ListKeyValuePairs -> M.toList <$> readMap
    Lookup k -> M.lookup k <$> readMap
    InsertOrLookup k v ->
      modifyMap \m ->
        return case M.lookup k m of
          Just v' -> (m, Just v')
          Nothing -> (M.insert k v m, Nothing)
    InsertOrReplace k v ->
      modifyMap_ \m -> do
        putIO k v
        return (M.insert k v m)
    InsertNewKey v ->
      modifyMap \m -> do
        let k = newUnique . map fst . M.toList $ m
        putIO k v
        return (M.insert k v m, k)
    Delete k ->
      modifyMap \m -> do
        deleteIO k
        return (M.delete k m, M.lookup k m)
    Reload -> modifyMap_ . const $ readIO
    Clear -> modifyMap_ . const $ clearIO $> M.empty

runKeyStoreCachedDB ::
  forall k v r a.
  (P.Embed IO :> r, Ord k) =>
  (SQL.Connection -> IO (Map k v)) ->
  (k -> v -> SQL.Connection -> IO ()) ->
  (k -> SQL.Connection -> IO ()) ->
  (SQL.Connection -> IO ()) ->
  P.Sem (KeyStore k v : r) a ->
  P.Sem r a
runKeyStoreCachedDB initSQL putSQL deleteSQL clearSQL =
  runKeyStoreCachedIO
    ( SQL.open "shinobu.db" >>= initSQL,
      \k v -> SQL.open "shinobu.db" >>= putSQL k v,
      \k -> SQL.open "shinobu.db" >>= deleteSQL k,
      SQL.open "shinobu.db" >>= clearSQL
    )
