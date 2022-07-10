module Shinobu.KeyStoreCommands where

import Calamity.Commands
import Calamity.Commands.Context (FullContext)
import qualified Polysemy as P
import qualified Shinobu.Effects.KeyStore as Id
import Shinobu.Types
import Shinobu.Util

data KeyStoreSpec = KeyStoreSpec
  { groupName :: Text,
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
--   help (const [i|Add a new automatic response|])
--     . command @params "add"
--     $ \ctx ->
--       f ctx >>> \fRet -> void do
--         val <- fRet
--         id_ <- Id.insertNewKey val
--         tellSuccess ctx (showItem id_ val)

mkListCommand ::
  (ShinobuC r, Id.KeyStore k v :> r) =>
  KeyStoreSpec ->
  (k -> v -> Text) ->
  P.Sem r (Command FullContext)
mkListCommand spec showItem =
  help (const [i|List all #{spec ^. #itemPlural}|])
    . command @'[] "list"
    $ \ctx -> void do
      items <- Id.listKeyValuePairs
      if null items
        then tellInfo ctx [i|No automatic responses exist. Use #{fmtCmd "response add"} to add one.|]
        else tellInfo ctx $ unlines $ items <&> uncurry showItem

mkDeleteCommand ::
  forall kp k v r.
  (ShinobuC r, Id.KeyStore k v :> r, ParameterParser kp FullContext r, ParserResult kp ~ k) =>
  KeyStoreSpec ->
  (k -> v -> Text) ->
  P.Sem r (Command FullContext)
mkDeleteCommand spec showItem = help (const [i|Delete an #{spec ^. #itemSingular}|])
  . commandA @'[Named "id" kp] "delete" ["remove", "rm"]
  $ \ctx id_ -> void do
    Id.delete id_ >>= \case
      Nothing ->
        tellError
          ctx
          [i|I couldn't find a #{spec ^. #itemSingular} with that id. Try listing them with #{fmtCmd (spec ^. #groupName <> " list")}|]
      Just val -> do
        tellSuccess
          ctx
          [i|Successfully removed the following #{spec ^. #itemSingular}:\n#{showItem id_ val}|]

mkReloadCommand ::
  (ShinobuC r, Id.KeyStore k v :> r) =>
  KeyStoreSpec ->
  P.Sem r (Command FullContext)
mkReloadCommand spec =
  help (const [i|Reload the #{spec ^. #itemSingular} database|])
    . command @'[] "reload"
    $ \ctx -> void do
      Id.reload
      tellSuccess ctx [i|Successfully synchronized my #{spec ^. #itemSingular} cache with the database.|]
