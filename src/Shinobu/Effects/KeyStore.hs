module Shinobu.Effects.KeyStore where

import qualified Data.Map.Strict as M
import qualified Polysemy as P
import qualified Polysemy.State as P

-- | A key value store
data KeyStore k v :: P.Effect where
  ListK :: KeyStore k v m [(k, v)]
  GetK :: k -> KeyStore k v m (Maybe v)
  PutK :: k -> v -> KeyStore k v m ()
  DeleteK :: k -> KeyStore k v m ()

P.makeSem ''KeyStore

-- | InMemory implementation of key value store
runKeyStoreAsState ::
  Ord k =>
  P.Sem (KeyStore k v : r) a ->
  P.Sem (P.State (M.Map k v) : r) a
runKeyStoreAsState = P.reinterpret \case
  ListK -> M.toList <$> P.get
  GetK k -> M.lookup k <$> P.get
  PutK k v -> P.modify $ M.insert k v
  DeleteK k -> P.modify $ M.delete k

runKeyStorePurely ::
  Ord k =>
  M.Map k v ->
  P.Sem (KeyStore k v : r) a ->
  P.Sem r (M.Map k v, a)
runKeyStorePurely kvMap = P.runState kvMap . runKeyStoreAsState

runKeyStoreIORef ::
  (Ord k, P.Embed IO :> r) =>
  IORef (M.Map k v) ->
  P.Sem (KeyStore k v : r) a ->
  P.Sem r a
runKeyStoreIORef ref = P.runStateIORef ref . runKeyStoreAsState
