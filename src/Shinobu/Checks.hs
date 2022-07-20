module Shinobu.Checks where

import Calamity
import Calamity.Commands.Context (FullContext)
import Data.Flags ((.~.))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated
import qualified Polysemy as P
import Shinobu.DB ()
import qualified Shinobu.Effects.DB as DB

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

isBotOwnerCtx :: (BotC r, DB.SQLite :> r) => FullContext -> P.Sem r (Maybe Text)
isBotOwnerCtx ctx = do
  let id_ = ctx ^. #user % #id
  owners <- map SQL.fromOnly <$> DB.run [iquery|SELECT id FROM user WHERE is_owner|]
  return
    if id_ `elem` owners
      then Nothing
      else Just "You have to be a bot owner to use this command."
