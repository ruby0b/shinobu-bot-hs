module Shinobu.Utils.Checks where

import Calamity
import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import Data.Flags ((.~.))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated
import qualified Polysemy as P
import qualified Shinobu.Effects.DB as DB
import Shinobu.Utils.DB ()

requiresAdmin = requires' "Admin" isAdminCtx

requiresOwner = requires' "Owner" isBotOwnerCtx

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

isBotOwnerCtx :: (BotC r, DB.DB :> r) => FullContext -> P.Sem r (Maybe Text)
isBotOwnerCtx ctx = do
  let id_ = ctx ^. #user % #id
  owners <- map SQL.fromOnly <$> DB.query [isql|SELECT id FROM user WHERE is_owner|]
  return
    if id_ `elem` owners
      then Nothing
      else Just "You have to be a bot owner to use this command."
