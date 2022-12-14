module Shinobu.Effects.Cache where

import qualified Polysemy as P
import qualified Polysemy.State as P

data Cache s :: P.Effect where
  -- | get the cached value
  Cached :: Cache s n s
  -- | refresh the cache
  Refresh :: Cache s m ()

P.makeSem ''Cache

cacheAsState :: forall s r a. P.Sem r s -> P.Sem (Cache s : r) a -> P.Sem (P.State s : r) a
cacheAsState fetch =
  P.reinterpret \case
    Cached -> P.get
    Refresh -> P.raise fetch >>= P.put

evalCacheViaState :: forall s r. P.Sem r s -> P.InterpreterFor (Cache s) r
evalCacheViaState fetch sem = do
  s <- fetch
  P.evalState s . cacheAsState fetch $ sem
