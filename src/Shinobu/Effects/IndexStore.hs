module Shinobu.Effects.IndexStore where

import qualified Data.Map.Strict as M
import qualified Polysemy as P
import qualified Shinobu.Effects.KeyStore as Id

class Ord (Key a) => HasKey a where
  type Key a :: Type
  getKey :: a -> Key a

matchesKey :: HasKey a => a -> Key a -> Bool
matchesKey = (==) . getKey

sameKey :: HasKey a => a -> a -> Bool
sameKey = flip matchesKey . getKey

toKeyMap ::
  (Functor f, Foldable f, HasKey a) =>
  f a ->
  M.Map (Key a) a
toKeyMap = fromList . toList . fmap \p -> (getKey p, p)

-- | A store of items that each have a key (which can be computed from the item)
data IndexStore t :: P.Effect where
  ListI :: HasKey t => IndexStore t m [t]
  GetI :: HasKey t => Key t -> IndexStore t m (Maybe t)
  PutI :: HasKey t => t -> IndexStore t m ()
  DeleteI :: HasKey t => Key t -> IndexStore t m (Maybe t)

P.makeSem ''IndexStore

runIndexStoreAsKeyStore ::
  P.Sem (IndexStore v : r) a ->
  P.Sem (Id.KeyStore (Key v) v : r) a
runIndexStoreAsKeyStore = P.reinterpret \case
  ListI -> map snd <$> Id.listKeyValuePairs
  GetI k -> Id.lookup k
  PutI v -> Id.insertOrReplace (getKey v) v
  DeleteI k -> Id.delete k
