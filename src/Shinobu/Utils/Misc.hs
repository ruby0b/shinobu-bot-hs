module Shinobu.Utils.Misc where

import Calamity
import Calamity.Commands (help)
import Calamity.Types.LogEff (LogEff)
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as Colour
import Data.Foldable (maximum)
import Data.List.NonEmpty (groupBy)
import qualified Data.Text.Encoding as T
import qualified DiPolysemy as P
import qualified Polysemy as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Conc as P
import qualified Polysemy.Error as P
import qualified Polysemy.Fail as P
import qualified Polysemy.Reader as P
import Relude.Extra.Enum (safeToEnum)
import System.Clock

class Foldable f => Optional f where
  (//) :: f a -> a -> a
  (//) = flip (foldr const)
  infixr 1 //

instance Optional Maybe

instance Optional (Either e)

instance Optional []

handleFailByLogging :: LogEff :> r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging =
  P.runFail >=> \case
    Left e -> P.error (fromString e)
    _ -> return ()

fromRightM :: Applicative f => (a -> f b) -> Either a b -> f b
fromRightM = flip either pure

fromJustM :: Applicative f => f a -> Maybe a -> f a
fromJustM = flip maybe pure

maybeThrow :: forall e a r. P.Error e :> r => e -> Maybe a -> P.Sem r a
maybeThrow = fromJustM . P.throw

leftThrow :: forall e a b r. P.Error e :> r => (a -> e) -> Either a b -> P.Sem r b
leftThrow leftToErr = fromRightM (P.throw . leftToErr)

runAtomicStateNewTVarIO ::
  P.Embed IO :> r =>
  a ->
  P.Sem (P.AtomicState a : r) b ->
  P.Sem r b
runAtomicStateNewTVarIO val sem = do
  tvar <- newTVarIO val
  P.runAtomicStateTVar tvar sem

maybeSucc :: (Bounded a, Enum a) => a -> Maybe a
maybeSucc = safeToEnum . succ . fromEnum

tellEmbedWithColor :: (BotC r, Tellable t) => Colour.Colour Double -> t -> Text -> P.Sem r (Either RestError Message)
tellEmbedWithColor color t msg =
  tell @Embed t $
    def
      & #description ?~ msg
      & #color ?~ color

tellInfo = tellEmbedWithColor Colour.violet

tellSuccess = tellEmbedWithColor Colour.limegreen

tellError = tellEmbedWithColor Colour.red

messageLink :: Message -> Text
messageLink m =
  let gld = maybe "@me" (show @Text . fromSnowflake) (m ^. #guildID)
      chn = show @Text $ fromSnowflake $ getID @Channel m
      msg = show @Text $ fromSnowflake $ getID @Message m
   in [i|https://discord.com/channels/#{gld}/#{chn}/#{msg}|]

help_ :: P.Reader (c -> Text) :> r => Text -> P.Sem r a -> P.Sem r a
help_ = help . const

fmtCmd :: Text -> Text
fmtCmd = codeline . ("=" <>)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

shareFst :: NonEmpty (a, b, c) -> (a, NonEmpty (b, c))
shareFst ts = (fst3 $ head ts,) $ (\(_, y, z) -> (y, z)) <$> ts

-- | return value is sorted by the first tuple element in ascending order
indexByFst :: Ord a => [(a, b, c)] -> [(a, NonEmpty (b, c))]
indexByFst = map shareFst . groupBy ((==) `on` fst3) . sortOn fst3

maximumOr :: (Ord p, Foldable f) => p -> f p -> p
maximumOr defaultVal xs
  | null xs = defaultVal
  | otherwise = maximum xs

whenNothingRun :: Monad m => Maybe a -> m b -> m (Maybe a)
whenNothingRun (Just a) _ = pure (Just a)
whenNothingRun Nothing f = f >> pure Nothing

fmtNanosecondsAsSeconds :: Integer -> Text
fmtNanosecondsAsSeconds nano =
  let secs = nano `div` 10 ^ 9
      subsecs = nano - (secs * 10 ^ 9)
   in [i|Time: #{secs}.#{subsecs} s|]

timeit :: (BotC r, Tellable c) => c -> P.Sem r a -> P.Sem r a
timeit c io = do
  t <- timeitBegin
  !x <- io
  timeitEnd c t
  return x

timeit' :: (BotC r, Tellable c, NFData a) => c -> P.Sem r a -> P.Sem r a
timeit' c io = do
  t <- timeitBegin
  x <- force <$> io
  timeitEnd c t
  return x

timeitBegin :: P.Embed IO :> r => P.Sem r TimeSpec
timeitBegin = P.embed $ getTime Monotonic

timeitEnd :: (BotC r, Tellable c) => c -> TimeSpec -> P.Sem r ()
timeitEnd c t = void do
  t' <- P.embed $ getTime Monotonic
  let dnano = toNanoSecs $ diffTimeSpec t t'
  tellInfo c $ fmtNanosecondsAsSeconds dnano

stringErrorToFail :: P.Fail :> r => P.Sem (P.Error String : r) a -> P.Sem r a
stringErrorToFail =
  P.runError >=> \case
    Left e -> fail e
    Right v -> return v

runSyncInIO :: [P.Final IO, P.Embed IO] :>> r => P.Sem (P.Sync d : P.Race : r) a -> P.Sem r a
runSyncInIO = P.interpretRace . P.interpretSync

readFileTextP :: [P.Embed IO, P.Error String] :>> r => String -> P.Sem r Text
readFileTextP fp =
  fromRightM (P.throw . (("Error while decoding " <> fp) <>) . show)
    . T.decodeUtf8'
    =<< readFileBS fp
