module Shinobu.Utils.Calamity where

import Calamity
import Calamity.Metrics.Eff
import Calamity.Types.LogEff
import Control.Concurrent (threadDelay)
import qualified Control.Foldl as L
import Data.Bits (shiftL, shiftR)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import qualified Data.Vector as V
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Streaming as S
import qualified Streaming.NonEmpty as NES
import qualified Streaming.Prelude as S

getManyMessages ::
  ( HasID Channel c,
    P.Members '[P.Error RestError, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  Int ->
  c ->
  S.Stream (S.Of Message) (P.Sem r) ()
getManyMessages amount c =
  let getMessages f l = invoke $ GetChannelMessages c f (Just $ ChannelMessagesLimit $ toInteger l)
      iter (_, limit, filter_) =
        if limit <= 0
          then return (empty, limit, filter_)
          else do
            let retrieve = if limit >= 100 then 100 else limit
            ms <- V.fromList <$> (P.fromEither =<< getMessages filter_ retrieve)
            let limit' = if length ms < retrieve then 0 else limit - retrieve
            let filter_' = Just $ ChannelMessagesBefore $ view #id $ V.last ms
            return (ms, limit', filter_')
   in S.iterateM iter (pure (empty, amount, Nothing))
        & S.delay 1
        & S.map (\(x, _, _) -> x)
        & S.concat

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
  forall c m r s.
  HasID Channel c =>
  HasID Message m =>
  P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r =>
  c ->
  S.Stream (S.Of m) (P.Sem r) s ->
  P.Sem r (S.Of (Either RestError ()) s)
deleteManyMessages channel messages = do
  now <- P.embed getCurrentTime
  messages
    & S.span (youngerThan @Message twoWeeks now)
    & deleteManyMessagesInBulks channel
    >>= secondA (deleteManyMessagesOneByOne channel)
    <&> ofFold1

deleteManyMessagesInBulks ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  S.Stream (S.Of m) (P.Sem r) s ->
  P.Sem r (S.Of (Either RestError ()) s)
deleteManyMessagesInBulks c =
  delayedChunks 100 1 $
    S.inspect
      >=> either
        -- TODO impossible left case https://gitlab.com/global-access-public/streaming-nonempty/-/issues/1
        (return . (Right () S.:>))
        \(m S.:> ms) ->
          S.toList ms
            >>= firstA \case
              [] -> invoke $ DeleteMessage c m
              (m' : ms'') -> invoke $ BulkDeleteMessages c (m : m' : ms'')

deleteManyMessagesOneByOne ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  S.Stream (S.Of m) (P.Sem r) s ->
  P.Sem r (S.Of (Either RestError ()) s)
deleteManyMessagesOneByOne c =
  -- TODO: don't think sleeping every 100 deletions is necessary here, discord.py does it
  delayedChunks 100 1 $
    S.mapM (invoke . DeleteMessage c)
      >>> L.purely S.fold firstLeft

delayedChunks ::
  (Monoid b, MonadIO m) =>
  Int ->
  Double ->
  (forall r'. S.Stream (S.Of a) m r' -> m (S.Of (Either e b) r')) ->
  S.Stream (S.Of a) m r ->
  m (S.Of (Either e b) r)
delayedChunks chunkSize delay processChunk s =
  s & S.chunksOf chunkSize
    & S.mapped processChunk
    & S.delay delay
    & L.purely S.fold firstLeft

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
inspectNE :: NES.NEStream f m r -> f (S.Stream f m r)
inspectNE (NES.NEStream s) = s
