module Shinobu.Effects.UserError where

import Calamity
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Utils.Misc

newtype UserErr = UserErr Text
  deriving newtype (IsString, Eq, Ord, Show)

type UserError = P.Error UserErr

runErrorTellEmbed :: forall s t a r. (BotC r, Tellable t, Show s) => t -> P.Sem (P.Error s : r) a -> P.Sem r ()
runErrorTellEmbed t =
  P.runError >=> \case
    Left msg -> void $ tellError t $ show msg
    Right _ -> return ()

runUserErrorTellEmbed :: (BotC r, Tellable t) => t -> P.Sem (UserError : r) a -> P.Sem r ()
runUserErrorTellEmbed = runErrorTellEmbed @UserErr

intoUserError :: forall s c r. (UserError :> r, Show s) => P.Sem (P.Error s : r) c -> P.Sem r c
intoUserError = P.runError >=> leftThrow (UserErr . show)
