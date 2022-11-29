module Shinobu.Effects.UserError where

import Calamity
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Utils.Misc

data SomeShinobuException = forall e. Exception e => SomeShinobuException e

deriving instance Show SomeShinobuException

deriving instance Exception SomeShinobuException

class DisplayError a where
  displayError :: a -> Text

newtype UserErr = UserErr Text deriving newtype (IsString)

newtype IntegrityErr = IntegrityErr Text deriving newtype (IsString)

instance DisplayError UserErr where
  displayError (UserErr err) = err

instance DisplayError IntegrityErr where
  displayError (IntegrityErr err) = "Database Integrity Error: " <> err

instance DisplayError RestError where
  displayError = show

type UserError = P.Error UserErr

type IntegrityError = P.Error IntegrityErr

tellMyErrors :: (BotC r, Tellable t) => t -> P.Sem (UserError : IntegrityError : P.Error RestError : r) a -> P.Sem r ()
tellMyErrors t =
  runErrorTellEmbed @UserErr t
    >>> runErrorTellEmbed @IntegrityErr t
    >>> runErrorTellEmbed @RestError t

runErrorTellEmbed :: forall s t a r. (BotC r, Tellable t, DisplayError s) => t -> P.Sem (P.Error s : r) a -> P.Sem r ()
runErrorTellEmbed t =
  P.runError >=> \case
    Left msg -> void $ tellError t $ displayError msg
    Right _ -> return ()

intoUserError :: forall s c r. (UserError :> r, Show s) => P.Sem (P.Error s : r) c -> P.Sem r c
intoUserError = P.runError >=> leftThrow (UserErr . show)

intoSomeShinobuException :: forall s c r. (P.Error SomeShinobuException :> r, Exception s) => P.Sem (P.Error s : r) c -> P.Sem r c
intoSomeShinobuException = P.runError >=> leftThrow SomeShinobuException
