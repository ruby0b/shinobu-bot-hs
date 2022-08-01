module Shinobu.Utils.Streaming where

import Control.Concurrent (threadDelay)
import qualified Control.Foldl as L
import Streaming as S
import Streaming.Internal
import Streaming.NonEmpty
import qualified Streaming.Prelude as S

firstLeftWith :: (c -> b -> c) -> c -> L.Fold (Either a b) (Either a c)
firstLeftWith cat zero = L.Fold (either (const . Left) (fmap . cat)) (Right zero) identity

firstLeft :: Monoid b => L.Fold (Either a b) (Either a b)
firstLeft = firstLeftWith (<>) mempty

firstA :: (Bitraversable t, Applicative f) => (a -> f c) -> t a b -> f (t c b)
firstA f = bitraverse f pure

secondA :: (Bitraversable t, Applicative f) => (b -> f d) -> t a b -> f (t a d)
secondA = bitraverse pure

ofFold1 :: Semigroup a => S.Of a (S.Of a b) -> S.Of a b
ofFold1 (x S.:> (x' S.:> y)) = (x <> x') S.:> y

-- TODO contribute
inspectNE :: NEStream f m r -> f (Stream f m r)
inspectNE (NEStream s) = s

-- | Take elements up to and including the first element for which the predicate is False.
takeWhileIncluding :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhileIncluding thePred = loop
  where
    loop str = case str of
      Step (a :> as) -> if thePred a then Step (a :> loop as) else Step (a :> mempty)
      Effect m -> Effect (fmap loop m)
      Return _ -> Return ()
{-# INLINE takeWhileIncluding #-}

intersperseDelay :: MonadIO m => Double -> Stream (Of a) m r -> Stream (Of a) m r
intersperseDelay seconds str = case str of
  Return r -> Return r
  Effect m -> Effect (fmap (intersperseDelay seconds) m)
  Step (a :> rest) -> loop a rest
  where
    pico = truncate (seconds * 1000000)
    loop !a theStr = case theStr of
      Return r -> Step (a :> Return r)
      Effect m -> Effect (fmap (loop a) m)
      Step (b :> rest) -> do
        S.yield a
        liftIO $ putStrLn $ "Sleeping for " <> show seconds <> " seconds..."
        liftIO $ threadDelay pico
        loop b rest
{-# INLINEABLE intersperseDelay #-}
