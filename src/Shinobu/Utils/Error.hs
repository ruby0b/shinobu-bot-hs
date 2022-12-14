{-# OPTIONS_GHC -Wno-orphans #-}

module Shinobu.Utils.Error where

import Calamity
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Utils.Misc
import qualified Text.RE.TDFA as TDFA

class TryFromP source target r where
  tryFromP :: source -> P.Sem r target

instance TryFromP a a r where
  tryFromP = pure

tryIntoP :: forall target source r. TryFromP source target r => source -> P.Sem r target
tryIntoP = tryFromP

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
intoUserError = P.runError >=> fromRightThrow (UserErr . show)

intoSomeShinobuException :: forall e c r. (P.Error SomeShinobuException :> r, Exception e) => P.Sem (P.Error e : r) c -> P.Sem r c
intoSomeShinobuException = P.runError >=> fromRightThrow SomeShinobuException

newtype RegexCompilationException = RegexCompilationException Text
  deriving stock (Show)
  deriving anyclass (Exception)

compileRegexErr :: (P.Error RegexCompilationException :> r) => String -> P.Sem r TDFA.RE
compileRegexErr = P.fromEither . first RegexCompilationException . TDFA.compileRegex
