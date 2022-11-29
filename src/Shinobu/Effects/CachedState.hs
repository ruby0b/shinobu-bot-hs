module Shinobu.Effects.CachedState where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import GHC.IO.Handle (hDuplicate)
import GHC.IO.Handle.Lock (LockMode (SharedLock), hLock, hUnlock)
import qualified Polysemy as P
import qualified Polysemy.Conc as PC
import qualified Polysemy.Error as P
import qualified Polysemy.Resource as P
import qualified Polysemy.State as P
import Shinobu.Effects.UserError
import Shinobu.Utils.JSON
import System.IO (SeekMode (..), hClose, hSeek, openBinaryFile, openFile)
import Prelude hiding (modify)

data MState s :: P.Effect where
  MState :: (s -> m (s, a)) -> MState s m a
  MRead :: MState s m s

P.makeSem ''MState

runMStateIORef ::
  forall s r.
  P.Member (P.Embed IO) r =>
  IORef s ->
  P.InterpreterFor (MState s) r
runMStateIORef ref = P.interpretH \case
  MState f -> do
    x <- P.embed $ readIORef ref
    res <- P.runTSimple (f x)
    P.Inspector ins <- P.getInspectorT
    for_ (ins res) \(x', _) ->
      P.embed $ writeIORef ref x'
    pure (snd <$> res)
  MRead -> P.embed (readIORef ref) >>= P.pureT

-- runMStateScopedIORef ::
--   forall s r.
--   P.Member (PC.Scoped_ (P.Embed IO)) r =>
--   IORef s ->
--   P.InterpreterFor (MState s) r
-- runMStateScopedIORef ref = P.interpretH \case
--   MState f -> PC.scoped_ do
--     x <- P.embed $ readIORef ref
--     res <- P.runTSimple (f x)
--     P.Inspector ins <- P.getInspectorT
--     for_ (ins res) \(x', _) ->
--       P.embed $ writeIORef ref x'
--     pure (snd <$> res)
--   MRead -> PC.scoped_ (P.embed (readIORef ref)) >>= P.pureT

-- TODO: huh, this is almost MState (plus the ability to ignore the physical IO read/write)...
-- https://hackage.haskell.org/package/polysemy-conc-0.10.0.0/docs/Polysemy-Conc.html#v:interpretScopedWithH
data CachedState s :: P.Effect where
  -- | Read the resource and cache the value.
  Read :: CachedState s m s
  -- | Write a value to the resource.
  Write :: s -> CachedState s m ()
  -- | Read the resource, pass it to a function, then write the output and return some value.
  ModifyM :: (s -> m (s, a)) -> CachedState s m a
  -- | Get the cached state.
  Cached :: CachedState s m s

P.makeSem ''CachedState

modifyM_ :: CachedState t :> r => (t -> P.Sem r t) -> P.Sem r ()
modifyM_ f = modifyM \x -> do
  x' <- f x
  return (x', ())

modify :: CachedState a :> r => (a -> (a, b)) -> P.Sem r b
modify f = modifyM (pure . f)

modify_ :: CachedState a :> r => (a -> a) -> P.Sem r ()
modify_ f = modifyM_ (pure . f)

runCachedStateAsFile ::
  forall s r.
  [PC.Sync s, PC.Mask, P.Resource] :>> r =>
  (s -> ByteString) ->
  (ByteString -> P.Sem r s) ->
  FilePath ->
  P.InterpreterFor (CachedState s) r
runCachedStateAsFile toByteString fromByteStringM filename = undefined

-- runCachedStateAsScopedState
--   >>> (P.interceptH \case )
--   >>> runScopedStateAsLockedFile filename

runCachedStateAsJsonFile ::
  forall s r.
  [ PC.Sync s,
    PC.Mask,
    P.Resource,
    P.Error FromJSONException
  ]
    :>> r =>
  (ToJSON s, FromJSON s) =>
  FilePath ->
  P.InterpreterFor (CachedState s) r
runCachedStateAsJsonFile = runCachedStateAsFile (toStrict . encode) (P.fromEither . first FromJSONException . eitherDecodeStrict)

runCachedStateAsJsonFileVia ::
  forall j s r.
  [ PC.Sync s,
    PC.Mask,
    P.Resource,
    P.Error FromJSONException,
    P.Error (TryFromException j s)
  ]
    :>> r =>
  (From s j, TryFrom j s, ToJSON j, FromJSON j) =>
  FilePath ->
  P.InterpreterFor (CachedState s) r
runCachedStateAsJsonFileVia = runCachedStateAsFile (encodeVia @j) (decodeVia @j)

runCachedStateAsJsonFileVia' ::
  forall j s r.
  [ PC.Sync s,
    PC.Mask,
    P.Resource,
    P.Error SomeShinobuException
  ]
    :>> r =>
  (From s j, TryFrom j s, ToJSON j, FromJSON j, Show j, Typeable j, Typeable s) =>
  FilePath ->
  P.InterpretersFor [CachedState s, P.Error FromJSONException, P.Error (TryFromException j s)] r
runCachedStateAsJsonFileVia' filepath =
  runCachedStateAsJsonFileVia @j filepath
    >>> intoSomeShinobuException @FromJSONException
    >>> intoSomeShinobuException @(TryFromException j s)

runCachedStateAsScopedState ::
  forall s r.
  P.Members [PC.Sync s, PC.Mask, P.Resource, PC.Scoped_ (P.State s)] r =>
  P.InterpreterFor (CachedState s) r
runCachedStateAsScopedState = undefined

-- runCachedStateAsScopedState = P.interpretH \case
--   Read -> do
--     ret <- PCS.modify $
--       const $ PC.scoped_ do
--         x <- P.get
--         return (x, x)
--     P.pureT ret
--   Write x -> do
--     ret <- PCS.modify_ $
--       const $ PC.scoped_ do
--         P.put x
--         return x
--     P.pureT ret
--   ModifyM f ->
--     ( PCS.modify $
--         ( const $
--             ( PC.scoped_
--                 ( do
--                     x <- P.get
--                     res <- P.runTSimple (f x)
--                     P.Inspector ins <- P.getInspectorT
--                     for_ (ins res) \(x', _) -> P.put x'
--                     pure (snd <$> res)
--                 )
--             )
--         )
--     )
--       >>= P.pureT
--   Cached -> PCS.block >>= P.pureT

runScopedStateAsLockedFile ::
  forall r.
  [PC.Lock, P.Resource, P.Embed IO] :>> r =>
  FilePath ->
  P.InterpreterFor (PC.Scoped_ (P.State ByteString)) r
runScopedStateAsLockedFile filename =
  PC.interpretScoped allocator handler
  where
    allocator :: () -> (Handle -> P.Sem r x) -> P.Sem r x
    allocator () use = PC.lock $ P.bracket alloc dealloc use
    alloc :: P.Sem r Handle
    alloc = do
      h <- P.embed $ openBinaryFile filename ReadWriteMode
      P.embed $ hLock h SharedLock
      return h
    dealloc :: Handle -> P.Sem r ()
    dealloc = P.embed . hClose
    handler :: Handle -> P.State ByteString m x -> P.Sem r x
    handler h = \case
      P.Get -> P.embed do
        -- duplicate the handle because hGetContents closes it
        content <- hDuplicate h >>= BS.hGetContents
        hSeek h AbsoluteSeek 0
        return content
      P.Put x -> P.embed do
        BS.hPut h x
        hSeek h AbsoluteSeek 0
