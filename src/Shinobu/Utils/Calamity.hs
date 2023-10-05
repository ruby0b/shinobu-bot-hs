module Shinobu.Utils.Calamity where

import Calamity
import Calamity.Metrics.Eff
import qualified Control.Foldl as L
import Data.Bits (shiftL, shiftR)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import qualified Data.Vector as V
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Utils.Misc
import qualified Shinobu.Utils.Streaming as S
import qualified Streaming as S
import qualified Streaming.Prelude as S

getManyMessages ::
  ( HasID Channel c,
    P.Members '[P.Error RestError, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  -- | Optional upper estimate of how many messages will be consumed.
  -- This is not a hard limit, it's just for efficiency's sake.
  -- The stream will continue yielding messages indefinitely anyway (as long as there are any left).
  Maybe Int ->
  c ->
  S.Stream (S.Of Message) (P.Sem r) ()
getManyMessages amount c =
  S.concat $ loop (Nothing, amount)
  where
    getMessages f l = invoke $ GetChannelMessages c f (Just $ ChannelMessagesLimit $ toInteger l)
    loop (filter_, limitM) = do
      let retrieve = maybe 100 (min 100) limitM
      ms <- lift $ V.fromList <$> (P.fromEither =<< getMessages filter_ retrieve)
      S.yield ms
      let filter' = Just $ ChannelMessagesBefore (V.last ms ^. #id)
      let limit' = subtract retrieve <$> limitM
      unless (length ms < retrieve || maybe False (<= 0) limit') do
        sleep 1
        loop (filter', limit')

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
  P.Sem r (Either RestError (), s)
deleteManyMessages channel messages = do
  now <- P.embed getCurrentTime
  messages
    & S.span (youngerThan @Message twoWeeks now)
    & deleteManyMessagesInBulks channel
    >>= S.secondA (deleteManyMessagesOneByOne channel)
    <&> S.ofFold1
    <&> S.lazily

deleteManyMessagesInBulks ::
  ( HasID Channel c,
    HasID Message m,
    P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r
  ) =>
  c ->
  S.Stream (S.Of m) (P.Sem r) s ->
  P.Sem r (S.Of (Either RestError ()) s)
deleteManyMessagesInBulks c =
  -- loop undefined
  --   & L.purely S.fold S.firstLeft
  -- where
  --   loop [] = return ()
  --   loop ((m :| ms) : mss) = do
  --     res <-
  --       lift
  --         if null ms
  --           then invoke $ DeleteMessage c m
  --           else invoke $ BulkDeleteMessages c (m : ms)
  --     S.yield res
  --     unless (null mss) do
  --       sleep 1
  --       loop mss
  delayedChunks 100 1 $
    S.inspect
      >=> either
        -- TODO impossible left case https://gitlab.com/global-access-public/streaming-nonempty/-/issues/1
        (return . (Right () S.:>))
        \(m S.:> ms) ->
          S.toList ms
            >>= S.firstA \case
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
  -- TODO: don't think sleeping every 100 deletions is necessary here, but discord.py does it
  delayedChunks 100 1 $
    S.mapM (invoke . DeleteMessage c)
      >>> L.purely S.fold S.firstLeft

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
    & L.purely S.fold S.firstLeft

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
