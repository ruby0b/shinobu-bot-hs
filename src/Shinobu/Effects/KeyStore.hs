module Shinobu.Effects.KeyStore where

import Calamity
import qualified Data.Map.Strict as M
import qualified Database.SQLite.Simple as SQL
import qualified Polysemy as P
import qualified Polysemy.State as P
import qualified Shinobu.Effects.Cache as C
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

class KeyStoreC a k v where
  toKVList :: a -> [(k, v)]
  lookupK :: k -> a -> Maybe v

instance Ord k => KeyStoreC (M.Map k v) k v where
  toKVList = M.toList
  lookupK = M.lookup

-- | Doesn't require IO for reads to improve read speeds.
-- 'reload' reads back from IO, overwriting the MVar.
-- /Any action that modifies the map reloads the cache./
keyStoreToIOCache ::
  forall c k v r a.
  (C.Cache IO c :> r, KeyStoreC c k v) =>
  (k -> v -> IO (), k -> IO (), IO ()) ->
  P.Sem (KeyStore k v : r) a ->
  P.Sem r a
keyStoreToIOCache (putIO, deleteIO, clearIO) sem =
  sem & P.interpret \case
    ListKeyValuePairs -> toKVList <$> C.get
    Lookup k -> lookupK k <$> C.get
    InsertOrLookup k v -> C.modify \m -> do
      lookupK k m & \case
        Nothing -> putIO k v >> return Nothing
        Just v' -> return (Just v')
    InsertOrReplace k v -> C.put (putIO k v) (const ())
    InsertNewKey v -> C.modify \m -> do
      let k = newUnique . map fst . toKVList @_ @_ @v $ m
      putIO k v
      return k
    Delete k -> C.modify \m -> lookupK k m <$ deleteIO k
    Reload -> C.reload
    Clear -> C.modify $ const clearIO

runKeyStoreAsDBCache ::
  forall c k v r a.
  (P.Embed IO :> r, KeyStoreC c k v) =>
  (SQL.Connection -> IO c) ->
  (k -> v -> SQL.Connection -> IO ()) ->
  (k -> SQL.Connection -> IO ()) ->
  (SQL.Connection -> IO ()) ->
  P.Sem (KeyStore k v : C.Cache IO c : r) a ->
  P.Sem r a
runKeyStoreAsDBCache initSQL putSQL deleteSQL clearSQL =
  C.runCacheDB initSQL
    . keyStoreToIOCache
      ( \k v -> SQL.open "shinobu.db" >>= putSQL k v,
        \k -> SQL.open "shinobu.db" >>= deleteSQL k,
        SQL.open "shinobu.db" >>= clearSQL
      )
