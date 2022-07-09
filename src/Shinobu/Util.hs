module Shinobu.Util where

import Calamity
import Calamity.Commands.Context (FullContext)
import Calamity.Types.LogEff (LogEff)
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as Colour
import Data.Flags ((.~.))
import Data.Foldable (maximum)
import qualified DiPolysemy as P
import qualified Polysemy as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Error as P
import qualified Polysemy.Fail as P
import Relude.Extra.Enum (safeToEnum)

class Foldable f => Optional f where
  (//) :: f a -> a -> a
  (//) = flip (foldr const)
  infixr 1 //

instance Optional Maybe

instance Optional (Either e)

instance Optional []

handleFailByLogging :: LogEff :> r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> P.error (fromString e)
    _ -> pure ()

maybeThrow :: P.Error e :> r => e -> Maybe a -> P.Sem r a
maybeThrow failMsg maybeX = case maybeX of
  Just x -> pure x
  Nothing -> P.throw failMsg

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

isAdminCtx :: BotC r => FullContext -> P.Sem r (Maybe Text)
isAdminCtx ctx = do
  case ctx ^. #channel of
    GuildChannel' chan -> do
      perms <- permissionsIn' chan (ctx ^. #user)
      pure
        if perms .~. administrator
          then Nothing
          else Just "You have to be an administrator to use this command."
    _ -> pure $ Just "You can't use this command outside of a server."

fmtCmd :: Text -> Text
fmtCmd = codeline . ("=" <>)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

maximumOr :: (Ord p, Foldable f) => p -> f p -> p
maximumOr defaultVal xs
  | null xs = defaultVal
  | otherwise = maximum xs

whenNothingRun :: Monad m => Maybe a -> m b -> m (Maybe a)
whenNothingRun (Just a) _ = pure (Just a)
whenNothingRun Nothing f = f >> pure Nothing

defaultSafeSucc :: (Bounded a, Enum a, Eq a) => a -> Maybe a
defaultSafeSucc v = if v /= maxBound then Just (succ v) else Nothing
