module Shinobu.Commands.BannedPatterns where

import Calamity
import Calamity.Commands
import qualified Data.Map.Strict as M
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated
import qualified Shinobu.Effects.KeyStore as Id
import Shinobu.Utils.Checks
import Shinobu.Utils.KeyStoreCommands
import Shinobu.Utils.Misc
import Shinobu.Utils.Types
import Text.RE.TDFA

type PatternID = Integer

queryPatterns :: SQL.Connection -> IO (M.Map PatternID RE)
queryPatterns conn = do
  rawList :: [(PatternID, String)] <- conn & [iquery|SELECT * FROM banned_patterns|]
  M.fromList <$> forM rawList \(id_, rawRE) -> do
    regex <- compileRegex rawRE
    pure (id_, regex)

bannedPatterns :: ShinobuSem r
bannedPatterns = void
  . runSyncInIO
  . Id.runKeyStoreAsDBCache
    queryPatterns
    (\id_ pattern_ -> [iexecute|INSERT OR REPLACE INTO banned_patterns VALUES (${id_}, ${reSource pattern_})|])
    (\id_ -> [iexecute|DELETE FROM banned_patterns WHERE id=${id_}|])
    [iexecute|DELETE FROM banned_patterns|]
  $ do
    react @'MessageCreateEvt
      \(msg, _user, _member) -> do
        patterns <- Id.listKeyValuePairs
        let content = msg ^. #content
        forM_ patterns \(_id, pattern_) -> do
          let match = content ?=~ pattern_
          let isMatch = matched match
          when isMatch $ void do
            invoke $ DeleteMessage msg msg

    let spec = KeyStoreSpec {groupName = "banned", itemSingular = "banned regex pattern", itemPlural = "banned regex patterns"}

    help_ [i|Manage #{spec ^. #itemPlural}|]
      . requiresAdmin
      . group (spec ^. #groupName)
      $ do
        help_ [i|Add a new #{spec ^. #itemSingular}|]
          . command @'[Named "Regex to match" Text] "add"
          $ \ctx pattern_ -> void do
            regex <- compileRegex $ toString pattern_
            Id.insertNewKey regex
            tellSuccess ctx [i|Understood!\nI will now delete messages matching the pattern #{codeline pattern_}|]

        mkListCommand spec \id_ pattern_ ->
          [i|#{id_}: #{codeline $ fromString $ reSource $ pattern_}|]

        mkDeleteCommand @Integer spec \_id pattern_ ->
          [i|#{codeline $ fromString $ reSource $ pattern_}|]

        mkReloadCommand spec
