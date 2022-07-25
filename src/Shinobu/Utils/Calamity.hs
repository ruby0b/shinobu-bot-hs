{-# LANGUAGE NumericUnderscores #-}

module Shinobu.Utils.Calamity where

import Calamity
import Calamity.Metrics.Eff
import Calamity.Types.LogEff
import Control.Concurrent (threadDelay)
import Data.Bits (shiftL, shiftR)
import Data.List (partition)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import qualified Polysemy as P

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
  chunks 100
    >>> map (\(m :| ms) -> if null ms then invoke $ DeleteMessage c m else invoke $ BulkDeleteMessages c (m : ms))
    >>> intersperse (P.embed (threadDelay 1_000_000) $> Right ())
    >>> firstLeftM

deleteManyMessagesOneByOne ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  [m] ->
  P.Sem r (Either RestError ())
deleteManyMessagesOneByOne c =
  chunks 100
    >>> map (firstLeftM . map (invoke . DeleteMessage c) . toList)
    -- TODO: not sure if sleeping every 100 deletions is necessary here, discord.py does it
    >>> intersperse (P.embed (threadDelay 1_000_000) $> Right ())
    >>> firstLeftM

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

firstJustM :: (Foldable t, Monad m) => t (m (Maybe a)) -> m (Maybe a)
firstJustM = foldlM (fmap . (<|>)) Nothing

firstLeftM :: Monad m => [m (Either a ())] -> m (Either a ())
firstLeftM = maybeToLeft () <.> firstJustM . map (fmap leftToMaybe)

fromLeftM :: Monad m => m (Either a b) -> Either a b -> m (Either a b)
fromLeftM _ l@(Left _) = return l
fromLeftM right (Right _) = right

rightAndThenM :: Monad m => m (Either a b) -> m (Either a b) -> m (Either a b)
rightAndThenM x y = x >>= fromLeftM y

infixl 0 `rightAndThenM`
