module Shinobu.Utils.Misc where

import Calamity
import Calamity.Commands (help)
import Control.Concurrent (threadDelay)
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as Colour
import qualified DiPolysemy as P
import qualified Polysemy as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Conc as P
import qualified Polysemy.Error as P
import qualified Polysemy.Fail as P
import qualified Polysemy.Reader as P
import Relude.Extra.Enum (safeToEnum)
import System.Clock
import qualified Text.RE.TDFA as TDFA

class Foldable f => Optional f where
  (?:) :: f a -> a -> a
  (?:) = flip (foldr const)
  infixr 1 ?:

instance Optional Maybe

instance Optional (Either e)

instance Optional []

class Functor f => OptionalM f where
  (?!) :: f a -> P.Sem r a -> P.Sem r a
  infixr 1 ?!

class Bifunctor p => BioptionalM p where
  (?!>) :: p a b -> (a -> P.Sem r b) -> P.Sem r b
  infixr 1 ?!>

(>>?!) :: OptionalM f => P.Sem r (f a) -> P.Sem r a -> P.Sem r a
m >>?! f = m >>= (?! f)

infixr 1 >>?!

instance OptionalM Maybe where
  (?!) = whenNothing

instance OptionalM [] where
  (x : _) ?! _ = pure x
  [] ?! m = m

instance BioptionalM Either where
  e ?!> f = either f pure e

fromRightThrow :: P.Error e :> r => (a -> e) -> Either a b -> P.Sem r b
fromRightThrow f = either (P.throw . f) pure

handleFailByLogging :: LogEff :> r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging = P.runFail >=> either (P.error . fromString) (const $ pure ())

handleExceptionByLogging :: forall e r a. (LogEff :> r, Exception e) => P.Sem (P.Error e : r) a -> P.Sem r ()
handleExceptionByLogging = P.runError >=> either (P.error . displayException) (const $ pure ())

runAtomicStateNewTVarIO ::
  P.Embed IO :> r =>
  a ->
  P.Sem (P.AtomicState a : r) b ->
  P.Sem r b
runAtomicStateNewTVarIO val sem = do
  tvar <- newTVarIO val
  P.runAtomicStateTVar tvar sem

maybeSucc :: (Bounded a, Enum a) => a -> Maybe a
maybeSucc = safeToEnum . (+ 1) . fromEnum

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

fmtTDFA :: TDFA.RE -> Text
fmtTDFA = codeline . fromString . TDFA.reSource

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

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

runSyncInIO :: [P.Final IO, P.Embed IO] :>> r => P.Sem (P.Sync d : P.Race : r) a -> P.Sem r a
runSyncInIO = P.interpretRace . P.interpretSync

sleep :: MonadIO m => Double -> m ()
sleep seconds = liftIO $ threadDelay $ truncate (seconds * 1_000_000)

p = P.debug @Text . show

p' = P.debug @Text

debug = P.debug @Text

info = P.info @Text

warning = P.warning @Text
