module Shinobu.Utils.KeyStoreCommands where

import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Polysemy as P
import Shinobu.Effects.Cache
import Shinobu.Effects.DB
import Shinobu.Utils.Error
import Shinobu.Utils.Misc
import Shinobu.Utils.Types

data KeyStoreSpec = KeyStoreSpec
  { tableName :: Text,
    groupName :: Text,
    itemSingular :: Text,
    itemPlural :: Text
  }
  deriving (Show, Generic)

-- TODO: ask for CalamityCommands.CommandUtils.ListToTup to be exported to get this to work
-- mkAddCommand ::
--   forall (params :: [Type]) k v r.
--   ( ShinobuC r,
--     Id.KeyStore k v :> r,
--     ParameterParser (ListToTup params) FullContext r,
--     Id.NewUnique k
--   ) =>
--   KeyStoreSpec ->
--   (FullContext -> CommandForParsers params r v) ->
--   (k -> v -> Text) ->
--   P.Sem r (Command FullContext)
-- mkAddCommand spec f showItem =
--   help_ [i|Add a new automatic response|]
--     . command @params "add"
--     $ \ctx ->
--       f ctx >>> \fRet -> void do
--         val <- fRet
--         id_ <- Id.insertNewKey val
--         tellSuccess ctx (showItem id_ val)

mkListCommand ::
  (ShinobuC r, Cache [v] :> r) =>
  KeyStoreSpec ->
  (v -> Text) ->
  P.Sem r (Command FullContext)
mkListCommand spec showItem =
  help_ [i|List all #{spec ^. #itemPlural}|]
    . command @'[] "list"
    $ \ctx -> tellMyErrors ctx do
      items <- cached
      if null items
        then tellInfo ctx [i|No automatic responses exist. Use #{fmtCmd "response add"} to add one.|]
        else tellInfo ctx $ unlines $ items <&> showItem

mkDeleteCommand ::
  forall kparser sqlv k v r.
  ( ShinobuC r,
    Cache [v] :> r,
    SQL.ToField k,
    TryFromP sqlv v r,
    SQL.FromRow sqlv,
    ParameterParser kparser FullContext r,
    ParserResult kparser ~ k
  ) =>
  KeyStoreSpec ->
  (v -> Text) ->
  P.Sem r (Command FullContext)
mkDeleteCommand spec showItem = help_ [i|Delete an #{spec ^. #itemSingular}|]
  . commandA @'[Named "id" kparser] "delete" ["remove", "rm"]
  $ \ctx id_ -> tellMyErrors ctx do
    let table = into @String $ spec ^. #tableName
    deleted <- query @sqlv [isql|DELETE FROM !{table} WHERE id=${id_} RETURNING *|]
    case deleted of
      [] -> do
        tellError
          ctx
          [i|I couldn't find a #{spec ^. #itemSingular} with that id. Try listing them with #{fmtCmd (spec ^. #groupName <> " list")}|]
        return ()
      (val : _) -> do
        val' <- P.raise_ @r $ tryIntoP @v val
        tellSuccess
          ctx
          [i|Successfully removed the following #{spec ^. #itemSingular}:\n#{showItem val'}|]
        refresh

mkReloadCommand ::
  (ShinobuC r, Cache [v] :> r) =>
  KeyStoreSpec ->
  P.Sem r (Command FullContext)
mkReloadCommand spec =
  help_ [i|Reload the #{spec ^. #itemSingular} database|]
    . command @'[] "reload"
    $ \ctx -> tellMyErrors ctx do
      refresh
      tellSuccess ctx [i|Successfully synchronized my #{spec ^. #itemSingular} cache with the database.|]
