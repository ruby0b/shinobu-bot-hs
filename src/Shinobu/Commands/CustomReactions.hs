module Shinobu.Commands.CustomReactions where

import Calamity
import Calamity.Commands
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import Database.SQLite.Simple.QQ.Interpolated
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Polysemy as P
import qualified Polysemy.AtomicState as P
import Shinobu.Types
import Shinobu.Util
import Text.RE.TDFA

newtype PatternID = PatternID Int
  deriving newtype (Eq, Ord, Num, Show, SQL.FromField, SQL.ToField)

-- TODO abstract AtomicState into CachedAtomicState which automatically
-- updates the database on atomicModify
queryReactions :: SQL.Connection -> IO [(PatternID, RE, Text)]
queryReactions conn = do
  rawList :: [(PatternID, String, Text)] <- conn & [iquery|SELECT * FROM regex_reactions|]
  forM rawList \(id_, rawRE, response) -> do
    regex <- compileRegex rawRE
    pure (id_, regex, response)

addReaction :: (PatternID, RE, Text) -> SQL.Connection -> IO ()
addReaction (id_, pattern_, response) =
  [iexecute|INSERT INTO regex_reactions VALUES (${id_}, ${reSource pattern_}, ${response})|]

removeReaction :: PatternID -> SQL.Connection -> IO ()
removeReaction id_ = [iexecute|DELETE FROM regex_reactions WHERE id=${id_}|]

customReactions :: ShinobuSem r
customReactions = void
  . ( \sem -> do
        -- TODO obviously don't hardcode this.
        -- This should be in a reader.
        -- Or maybe make a DB effect to get a connection and
        -- make a convenience function for running a function that needs a connection
        conn <- P.embed $ SQL.open "shinobu.db"
        reactions <- P.embed $ queryReactions conn
        runAtomicStateNewTVarIO reactions sem
    )
  $ do
    react @'MessageCreateEvt
      \(msg, _user, _member) -> do
        reactions <- P.atomicGet @[(PatternID, RE, Text)]
        let content = msg ^. #content
        forM_ reactions \(_id, pattern_, response) -> do
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
            -- TODO: bla bla race conditions; should use something like atomicModify or whatever
            reactions <- P.atomicGet @[(PatternID, RE, Text)]
            let id_ = (+ 1) . maximumOr 0 . map fst3 $ reactions
            conn <- P.embed $ SQL.open "shinobu.db"
            P.embed $ addReaction (id_, regex, response) conn
            P.atomicModify' @[(PatternID, RE, Text)] ((id_, regex, response) :)
            tellSuccess ctx [i|Understood!\nI will now respond to the pattern #{codeline pattern_} by saying:\n#{quote response}|]

        help (const "List all automatic responses")
          . command @'[] "list"
          $ \ctx -> void do
            reactions <- P.atomicGet @[(PatternID, RE, Text)]
            if null reactions
              then tellInfo ctx [i|No automatic responses exist. Use #{fmtCmd "response add"} to add one.|]
              else
                tellInfo ctx $
                  unlines $
                    reactions <&> \(id_, pattern_, response) ->
                      [i|#{id_}: #{codeline $ fromString $ reSource $ pattern_}\n#{quote response}|]

        help (const "Delete an automatic response")
          . commandA @'[Named "id" Int] "delete" ["remove", "rm"]
          $ \ctx id_int -> void do
            let id_ = PatternID id_int
            reactions <- P.atomicGet @[(PatternID, RE, Text)]
            -- TODO obviously this also suffers from race conditions...
            let mReaction = find ((== id_) . fst3) reactions
            case mReaction of
              Nothing -> tellError ctx [i|I couldn't find a response with that id. Try listing them with #{fmtCmd "response list"}|]
              Just (_, pattern_, response) -> do
                conn <- P.embed $ SQL.open "shinobu.db"
                P.embed $ removeReaction id_ conn
                P.atomicModify' @[(PatternID, RE, Text)] (filter ((/= id_) . fst3))
                tellSuccess ctx $
                  [i|Successfully removed the following reaction:\n#{codeline $ fromString $ reSource $ pattern_}\n#{quote response}|]
