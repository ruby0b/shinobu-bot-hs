{-# OPTIONS_GHC -Wno-orphans #-}

module Shinobu.Utils.Error where

import Calamity
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Utils.Misc

data SomeShinobuException = forall e. Exception e => SomeShinobuException e

deriving instance Show SomeShinobuException

deriving instance Exception SomeShinobuException

instance Exception RestError

newtype UserErr = UserErr String
  deriving newtype (IsString)
  deriving stock (Show)
  deriving anyclass (Exception)

newtype IntegrityErr = IntegrityErr String
  deriving newtype (IsString)
  deriving stock (Show)

instance Exception IntegrityErr where
  displayException (IntegrityErr err) = "Database Integrity Error: " <> err

newtype DiscordErr = DiscordErr String
  deriving newtype (IsString)
  deriving stock (Show)
  deriving anyclass (Exception)

type UserError = P.Error UserErr

type IntegrityError = P.Error IntegrityErr

type DiscordError = P.Error DiscordErr

tellMyErrors :: (BotC r, Tellable t) => t -> P.Sem (UserError : IntegrityError : P.Error RestError : r) a -> P.Sem r ()
tellMyErrors t =
  runErrorTellEmbed @UserErr t
    >>> runErrorTellEmbed @IntegrityErr t
    >>> runErrorTellEmbed @RestError t

runErrorTellEmbed :: forall s t a r. (BotC r, Tellable t, Exception s) => t -> P.Sem (P.Error s : r) a -> P.Sem r ()
runErrorTellEmbed t =
  P.runError >=> \case
    Left msg -> void $ tellError t $ into @Text $ displayException msg
    Right _ -> return ()

intoUserError :: forall s c r. (UserError :> r, Show s) => P.Sem (P.Error s : r) c -> P.Sem r c
intoUserError = P.runError >=> leftThrow (UserErr . show)

intoSomeShinobuException :: forall s c r. (P.Error SomeShinobuException :> r, Exception s) => P.Sem (P.Error s : r) c -> P.Sem r c
intoSomeShinobuException = P.runError >=> leftThrow SomeShinobuException
