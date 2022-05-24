module Shinobu.Commands.BannedPatterns where

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

type PatternList = [(PatternID, RE)]

-- TODO abstract AtomicState into CachedAtomicState which automatically
-- updates the database on atomicModify
queryPatterns :: SQL.Connection -> IO PatternList
queryPatterns conn = do
  rawList :: [(PatternID, String)] <- conn & [iquery|SELECT * FROM banned_patterns|]
  forM rawList \(id_, rawRE) -> do
    regex <- compileRegex rawRE
    pure (id_, regex)

addPattern :: (PatternID, RE) -> SQL.Connection -> IO ()
addPattern (id_, pattern_) =
  [iexecute|INSERT INTO banned_patterns VALUES (${id_}, ${reSource pattern_})|]

removePattern :: PatternID -> SQL.Connection -> IO ()
removePattern id_ = [iexecute|DELETE FROM banned_patterns WHERE id=${id_}|]

bannedPatterns :: ShinobuSem r
bannedPatterns = void
  . ( \sem -> do
        -- TODO obviously don't hardcode this.
        -- This should be in a reader.
        -- Or maybe make a DB effect to get a connection and
        -- make a convenience function for running a function that needs a connection
        conn <- P.embed $ SQL.open "shinobu.db"
        patterns <- P.embed $ queryPatterns conn
        runAtomicStateNewTVarIO patterns sem
    )
  $ do
    react @'MessageCreateEvt
      \(msg, _user, _member) -> do
        patterns <- P.atomicGet @PatternList
        let content = msg ^. #content
        forM_ patterns \(_id, pattern_) -> do
          let match = content ?=~ pattern_
          let isMatch = matched match
          when isMatch $ void do
            invoke $ DeleteMessage msg msg

    help (const "Manage banned regex patterns")
      . requires' "Admin" isAdminCtx
      . group "banned"
      $ do
        help (const "Add a new banned pattern")
          . command @'[Named "Regex to match" Text] "add"
          $ \ctx pattern_ -> void do
            regex <- compileRegex $ toString pattern_
            -- TODO: bla bla race conditions; should use something like atomicModify or whatever
            patterns <- P.atomicGet @PatternList
            let id_ = (+ 1) . maximumOr 0 . map fst $ patterns
            conn <- P.embed $ SQL.open "shinobu.db"
            P.embed $ addPattern (id_, regex) conn
            P.atomicModify' @PatternList ((id_, regex) :)
            tellSuccess ctx [i|Understood!\nI will now delete messages matching the pattern #{codeline pattern_}|]

        help (const "List all banned patterns")
          . command @'[] "list"
          $ \ctx -> void do
            patterns <- P.atomicGet @PatternList
            if null patterns
              then tellInfo ctx [i|No banned patterns exist. Use #{fmtCmd "banned add"} to add one.|]
              else
                tellInfo ctx $
                  unlines $
                    patterns <&> \(id_, pattern_) ->
                      [i|#{id_}: #{codeline $ fromString $ reSource $ pattern_}|]

        help (const "Delete a banned pattern")
          . commandA @'[Named "id" Int] "delete" ["remove", "rm"]
          $ \ctx id_int -> void do
            let id_ = PatternID id_int
            patterns <- P.atomicGet @PatternList
            -- TODO obviously this also suffers from race conditions...
            let mPattern = find ((== id_) . fst) patterns
            case mPattern of
              Nothing -> tellError ctx [i|I couldn't find a banned pattern with that id. Try listing them with #{fmtCmd "banned list"}|]
              Just (_, pattern_) -> do
                conn <- P.embed $ SQL.open "shinobu.db"
                P.embed $ removePattern id_ conn
                P.atomicModify' @PatternList (filter ((/= id_) . fst))
                tellSuccess ctx $
                  [i|Successfully removed the following banned pattern:\n#{codeline $ fromString $ reSource $ pattern_}|]
