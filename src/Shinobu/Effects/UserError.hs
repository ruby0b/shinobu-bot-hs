module Shinobu.Effects.UserError where

import Calamity
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Util

newtype UserErr = UserErr Text
  deriving newtype (IsString, Eq, Ord)

type UserError = P.Error UserErr

runUserErrorTellEmbed :: (BotC r, Tellable t) => t -> P.Sem (UserError : r) a -> P.Sem r ()
runUserErrorTellEmbed t =
  P.runError >=> \case
    Left (UserErr msg) -> void $ tellError t msg
    Right _ -> return ()

intoUserError :: UserError :> r => P.Sem (P.Error Text : r) c -> P.Sem r c
intoUserError = P.runError >=> leftThrow UserErr
