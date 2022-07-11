module Shinobu.Effects.UserError where

import Calamity
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Util (tellError)

data UserError = forall t. Tellable t => UserError t Text

userError :: (P.Error UserError :> r, Tellable t) => t -> Text -> P.Sem r a
userError t msg = P.throw (UserError t msg)

maybeUserError :: (P.Member (P.Error UserError) r, Tellable t) => t -> Text -> Maybe a -> P.Sem r a
maybeUserError t msg = maybe (userError t msg) return

runUserErrorTellEmbed :: BotC r => P.Sem (P.Error UserError : r) a -> P.Sem r ()
runUserErrorTellEmbed =
  P.runError >=> \case
    Left (UserError t msg) -> void $ tellError t msg
    Right _ -> return ()
