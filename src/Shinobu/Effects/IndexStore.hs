module Shinobu.Effects.IndexStore where

import qualified Data.Map.Strict as M
import qualified Polysemy as P
import Shinobu.Effects.KeyStore

class (Eq (Key a)) => HasKey a where
  type Key a :: Type
  getKey :: a -> Key a

matchesKey :: HasKey a => a -> Key a -> Bool
matchesKey = (==) . getKey

sameKey :: HasKey a => a -> a -> Bool
sameKey = flip matchesKey . getKey

toKeyMap ::
  (Functor f, Foldable f, HasKey a, Ord (Key a)) =>
  f a ->
  M.Map (Key a) a
toKeyMap = fromList . toList . fmap \p -> (getKey p, p)

-- | A store of items that each have a key (which can be computed from the item)
data IndexStore t :: P.Effect where
  ListI :: HasKey t => IndexStore t m [t]
  GetI :: HasKey t => Key t -> IndexStore t m (Maybe t)
  PutI :: HasKey t => t -> IndexStore t m ()
  DeleteI :: HasKey t => Key t -> IndexStore t m ()

P.makeSem ''IndexStore

runIndexStoreAsKeyStore ::
  P.Sem (IndexStore v : r) a ->
  P.Sem (KeyStore (Key v) v : r) a
runIndexStoreAsKeyStore = P.reinterpret \case
  ListI -> map snd <$> listK
  GetI k -> getK k
  PutI v -> putK (getKey v) v
  DeleteI k -> deleteK k
