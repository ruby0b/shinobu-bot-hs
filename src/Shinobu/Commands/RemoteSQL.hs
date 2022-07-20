module Shinobu.Commands.RemoteSQL where

import Calamity
import Calamity.Commands
import qualified Database.SQLite.Simple as SQL
import Shinobu.Checks
import Shinobu.DB
import qualified Shinobu.Effects.DB as DB
import Shinobu.Parsers
import Shinobu.Types
import Shinobu.Util
import qualified Text.PrettyPrint.Boxes as B

remoteSQLCmd :: ShinobuSem r
remoteSQLCmd = void do
  help_ "Execute SQL queries remotely"
    . requires' "Owner" isBotOwnerCtx
    . command @'[Named "query" Code] "sql"
    $ \ctx queryCode -> void do
      rows :: [[ShowField]] <- DB.run $ flip SQL.query_ (fromString $ toString $ queryCode ^. #code)
      -- TODO print a pretty ascii table
      let table = unlines $ map (unwords . map showField) rows
      tellInfo ctx $ codeblock' Nothing table
