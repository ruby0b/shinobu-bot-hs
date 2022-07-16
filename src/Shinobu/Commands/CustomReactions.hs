module Shinobu.Commands.CustomReactions where

import Calamity
import Calamity.Commands
import qualified Data.Map.Strict as M
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated
import qualified Shinobu.Effects.KeyStore as Id
import Shinobu.KeyStoreCommands
import Shinobu.Types
import Shinobu.Util
import Text.RE.TDFA

type PatternID = Integer

queryReactions :: SQL.Connection -> IO (M.Map PatternID (RE, Text))
queryReactions conn = do
  rawList :: [(PatternID, String, Text)] <- conn & [iquery|SELECT * FROM regex_reactions|]
  M.fromList <$> forM rawList \(id_, rawRE, response) -> do
    regex <- compileRegex rawRE
    pure (id_, (regex, response))

customReactions :: ShinobuSem r
customReactions = void
  . runSyncInIO
  . Id.runKeyStoreAsDBCache
    queryReactions
    (\id_ (pattern_, response) -> [iexecute|INSERT OR REPLACE INTO regex_reactions VALUES (${id_}, ${reSource pattern_}, ${response})|])
    (\id_ -> [iexecute|DELETE FROM regex_reactions WHERE id=${id_}|])
    [iexecute|DELETE FROM regex_reactions|]
  $ do
    react @'MessageCreateEvt
      \(msg, _user, _member) -> do
        reactions <- Id.listKeyValuePairs
        let content = msg ^. #content
        forM_ reactions \(_id, (pattern_, response)) -> do
          let match = content ?=~ pattern_
          let isMatch = matched match
          when isMatch $ void do
            tell msg response

    let spec = KeyStoreSpec {groupName = "response", itemSingular = "automatic response", itemPlural = "automatic responses"}

    help_ [i|Manage #{spec ^. #itemPlural}|]
      . requires' "Admin" isAdminCtx
      . group (spec ^. #groupName)
      $ do
        help_ [i|Add a new #{spec ^. #itemSingular}|]
          . command @'[Named "Regex to match" Text, Named "My response" Text] "add"
          $ \ctx pattern_ response -> void do
            regex <- compileRegex $ toString pattern_
            Id.insertNewKey (regex, response)
            tellSuccess ctx [i|Understood!\nI will now respond to the pattern #{codeline pattern_} by saying:\n#{quote response}|]

        mkListCommand spec \id_ (pattern_, response) ->
          [i|#{id_}: #{codeline $ fromString $ reSource $ pattern_}\n#{quote response}|]

        mkDeleteCommand @Integer spec \_id (pattern_, response) ->
          [i|#{codeline $ fromString $ reSource $ pattern_}\n#{quote response}|]

        mkReloadCommand spec
