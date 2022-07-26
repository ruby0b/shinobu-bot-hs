{-# LANGUAGE NumericUnderscores #-}

module Shinobu.Utils.Calamity where

import Calamity
import Calamity.Metrics.Eff
import Calamity.Types.LogEff
import Control.Concurrent (threadDelay)
import Data.Bits (shiftL, shiftR)
import Data.List (partition)
import Data.Maybe (fromJust)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import qualified Polysemy as P
import qualified Polysemy.Error as P

getManyMessages ::
  ( HasID Channel c,
    P.Members '[P.Error RestError, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  Int ->
  c ->
  P.Sem r [Message]
getManyMessages amount c = do
  let (q, r) = amount `divMod` 100
      getMessages f l = invoke $ GetChannelMessages c f (Just $ ChannelMessagesLimit $ toInteger l)
  (f, ms) <-
    P.fromEither
      =<< foldr
        ( \_ acc -> do
            (f, ms) <- P.fromEither =<< acc
            ms' <- P.fromEither =<< getMessages f 100
            let m = view #id $ fromJust $ viaNonEmpty last ms'
            P.embed (threadDelay 1_000_000)
            return $ Right (Just $ ChannelMessagesBefore m, ms' ++ ms)
        )
        (return $ Right (Nothing, []))
        [1 .. q]
  ms' <- P.fromEither =<< getMessages f r
  return (ms ++ ms')

deleteMessage ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  m ->
  P.Sem r (Either RestError ())
deleteMessage c = invoke . DeleteMessage c

deleteManyMessages ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  [m] ->
  P.Sem r (Either RestError ())
deleteManyMessages channel messages = do
  now <- P.embed getCurrentTime
  let (newMessages, oldMessages) = partition (youngerThan @Message twoWeeks now) messages
  deleteManyMessagesInBulks channel newMessages
    `rightAndThenM` deleteManyMessagesOneByOne channel oldMessages

deleteManyMessagesInBulks ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  [m] ->
  P.Sem r (Either RestError ())
deleteManyMessagesInBulks c =
  delayedChunks 100 1_000_000 $
    \(m :| ms) ->
      if null ms
        then invoke $ DeleteMessage c m
        else invoke $ BulkDeleteMessages c (m : ms)

deleteManyMessagesOneByOne ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  [m] ->
  P.Sem r (Either RestError ())
deleteManyMessagesOneByOne c =
  -- TODO: not sure if sleeping every 100 deletions is necessary here, discord.py does it
  delayedChunks 100 1_000_000 $
    \ms -> firstLeftM $ invoke . DeleteMessage c <$> ms

delayedChunks ::
  (P.Embed IO :> r, Monoid m) =>
  Int ->
  Int ->
  (NonEmpty a -> P.Sem r (Either e m)) ->
  [a] ->
  P.Sem r (Either e m)
delayedChunks chunkSize delay processChunk =
  chunks chunkSize
    >>> map processChunk
    >>> intersperseDelays delay
    >>> firstLeftM

intersperseDelays :: (P.Embed IO :> r, Monoid b) => Int -> [P.Sem r (Either a b)] -> [P.Sem r (Either a b)]
intersperseDelays delay = intersperse (P.embed (threadDelay delay) $> Right mempty)

youngerThan :: forall t a. HasID t a => NominalDiffTime -> UTCTime -> a -> Bool
youngerThan diffTime now =
  let twoWeeksAgo = utcToMockSnowflake $ addUTCTime (- diffTime) now
   in (> twoWeeksAgo) . fromSnowflake . getID @t

twoWeeks :: NominalDiffTime
twoWeeks = 14 * 24 * 60 * 60

utcToMockSnowflake :: UTCTime -> Word64
utcToMockSnowflake = discordEpochToFakeSnowflake . utcToDiscordEpochMS

type DiscordEpoch = Word64

discordEpochToFakeSnowflake :: DiscordEpoch -> Word64
discordEpochToFakeSnowflake t = t `shiftL` 22

utcToDiscordEpochMS :: UTCTime -> DiscordEpoch
utcToDiscordEpochMS t = utcToUnix t * 1000 - 1420070400000

snowflakeToDiscordEpoch :: Snowflake t -> DiscordEpoch
snowflakeToDiscordEpoch s = (fromSnowflake s `shiftR` 22) + 1420070400000

utcToUnix :: UTCTime -> Word64
utcToUnix t = round (diffUTCTime t unixEpoch)

unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

-- adapted from https://stackoverflow.com/a/12882583/12425854
chunks :: Int -> [a] -> [NonEmpty a]
chunks n = takeWhileJust . map nonEmpty . infiniteUnfoldr (splitAt n)

infiniteUnfoldr :: (t -> (a, t)) -> t -> [a]
infiniteUnfoldr f = go
  where
    go b = let (a, new_b) = f b in a : go new_b

takeWhileJust :: [Maybe (NonEmpty a)] -> [NonEmpty a]
takeWhileJust = foldr (\x acc -> maybe [] (: acc) x) []

firstLeftM :: (Foldable t, Monad m, Monoid b) => t (m (Either a b)) -> m (Either a b)
firstLeftM = foldlM (either (const . return . Left) (fmap . fmap . (<>))) (Right mempty)

rightAndThenM :: Monad m => m (Either a b) -> m (Either a b) -> m (Either a b)
rightAndThenM x y = x >>= fromLeftM y

infixl 0 `rightAndThenM`

fromLeftM :: Monad m => m (Either a b) -> Either a b -> m (Either a b)
fromLeftM _ l@(Left _) = return l
fromLeftM right (Right _) = right
