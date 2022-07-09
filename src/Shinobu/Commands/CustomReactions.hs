module Shinobu.Commands.CustomReactions where

import Calamity
import Calamity.Commands
import qualified Data.Map.Strict as M
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated
import qualified Shinobu.Effects.KeyStore as Id
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
  . Id.runKeyStoreCachedDB
    queryReactions
    (\id_ (pattern_, response) -> [iexecute|INSERT INTO regex_reactions VALUES (${id_}, ${reSource pattern_}, ${response})|])
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

    help (const "Manage automatic responses")
      . requires' "Admin" isAdminCtx
      . group "response"
      $ do
        help (const "Add a new automatic response")
          . command @'[Named "Regex to match" Text, Named "My response" Text] "add"
          $ \ctx pattern_ response -> void do
            regex <- compileRegex $ toString pattern_
            Id.insertNewKey (regex, response)
            tellSuccess ctx [i|Understood!\nI will now respond to the pattern #{codeline pattern_} by saying:\n#{quote response}|]

        help (const "List all automatic responses")
          . command @'[] "list"
          $ \ctx -> void do
            reactions <- Id.listKeyValuePairs
            if null reactions
              then tellInfo ctx [i|No automatic responses exist. Use #{fmtCmd "response add"} to add one.|]
              else
                tellInfo ctx $
                  unlines $
                    reactions <&> \(id_, (pattern_, response)) ->
                      [i|#{id_}: #{codeline $ fromString $ reSource $ pattern_}\n#{quote response}|]

        help (const "Delete an automatic response")
          . commandA @'[Named "id" Integer] "delete" ["remove", "rm"]
          $ \ctx id_ -> void do
            mReaction <- Id.delete id_
            -- TODO obviously this also suffers from race conditions... (though it's not that harmful)
            case mReaction of
              Nothing -> tellError ctx [i|I couldn't find a response with that id. Try listing them with #{fmtCmd "response list"}|]
              Just (pattern_, response) -> do
                tellSuccess ctx $
                  [i|Successfully removed the following reaction:\n#{codeline $ fromString $ reSource $ pattern_}\n#{quote response}|]

        help (const "Reload the automatic response database")
          . command @'[] "reload"
          $ \ctx -> void do
            Id.reload
            tellSuccess ctx "Successfully synchronized my response cache with the database."
