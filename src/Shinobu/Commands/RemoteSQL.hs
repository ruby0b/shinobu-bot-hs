module Shinobu.Commands.RemoteSQL where

import Calamity
import Calamity.Commands
import Data.MonoTraversable (olength)
import qualified Database.SQLite.Simple as SQL
import qualified Shinobu.Effects.DB as DB
import Shinobu.Utils.Checks
import Shinobu.Utils.DB
import Shinobu.Utils.Misc
import Shinobu.Utils.Parsers
import Shinobu.Utils.Types
import qualified Text.PrettyPrint.Boxes as B

tableToBox :: [[String]] -> B.Box
tableToBox = B.hsep 2 B.left . map (B.vcat B.left . map B.text) . transpose

remoteSQLCmd :: ShinobuSem r
remoteSQLCmd = void do
  help_ "Execute SQL queries remotely"
    . requiresOwner
    . command @'[Named "query" Code] "sql"
    $ \ctx queryCode -> void do
      rows :: [[ShowField]] <- DB.run $ flip SQL.query_ (fromString $ toString $ queryCode ^. #code)
      if null rows
        then tellInfo ctx "Done! 0 rows were returned."
        else do
          let stringTable = map (map (toString . showField)) rows
          let rendered = into @Text $ B.render $ tableToBox stringTable
          let renderedLength = olength rendered
          if renderedLength <= 4096
            then tell ctx $ codeblock' Nothing rendered
            else tellError ctx [i|Response was too long (#{renderedLength} > 4096)|]
