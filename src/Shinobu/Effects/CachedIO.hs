module Shinobu.Effects.CachedIO where

import qualified Polysemy as P
import Shinobu.Util (runAtomicStateNewTVarIO)
import qualified Polysemy.AtomicState as P

data CachedIO s :: P.Effect where
  CachedState :: (s -> (s, a)) -> CachedIO a m a
  CachedGet :: CachedIO a m s

P.makeSem ''CachedIO

-- runCachedIOInitTVar ::
--   forall v r a.
--   P.Embed IO :> r =>
--   IO v ->
--   P.Sem (CachedIO v : r) a ->
--   P.Sem (P.AtomicState v : r) a
-- runCachedIOInitTVar genInitial sem = do
--   initialVal <- P.embed genInitial
--   sem & P.reinterpret \case
--     CachedState f -> P.atomicState @v f
--     CachedGet -> P.atomicGet @v
