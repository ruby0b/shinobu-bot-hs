module Shinobu.Commands.BannedPatterns where

import Calamity
import Calamity.Commands
import qualified Shinobu.Effects.CachedState as CS
import Shinobu.Utils.Checks
import Shinobu.Utils.JSON
import Shinobu.Utils.KeyStoreCommands
import Shinobu.Utils.Misc
import Shinobu.Utils.Parsers
import Shinobu.Utils.Types
import Text.RE.TDFA

newtype BannedPatternJSON = BannedPatternJSON {regex :: String} deriving (Generic, Show)

makeJSON ''BannedPatternJSON

newtype BannedPattern = BannedPattern {regex :: RE} deriving (Generic)

instance TryFrom [BannedPatternJSON] [BannedPattern] where
  tryFrom xs = tryFromWrapExc xs $ mapM (BannedPattern <.> compileRegexExc . view #regex) xs

instance From [BannedPattern] [BannedPatternJSON] where
  from = map (BannedPatternJSON . reSource . view #regex)

bannedPatterns :: ShinobuSem r
bannedPatterns = void
  . runSyncInIO
  . CS.runCachedStateAsJsonFileVia' @[BannedPatternJSON] "data/banned-patterns.json"
  $ do
    react @'MessageCreateEvt
      \(msg, _user, _member) -> do
        patterns <- CS.cached @[BannedPattern]
        let content = msg ^. #content
        forM_ patterns \pat -> do
          let match = content ?=~ (pat ^. #regex)
          let isMatch = matched match
          when isMatch $ void do
            invoke $ DeleteMessage msg msg

    let spec = KeyStoreSpec {groupName = "banned", itemSingular = "banned regex pattern", itemPlural = "banned regex patterns"}

    help_ [i|Manage #{spec ^. #itemPlural}|]
      . requiresAdmin
      . group (spec ^. #groupName)
      $ do
        help_ [i|Add a new #{spec ^. #itemSingular}|]
          . command @'[Named "pattern to match" RegExp] "add"
          $ \ctx (view #regex -> regex) -> void do
            CS.modify_ (BannedPattern regex :)
            tellSuccess ctx [i|Understood!\nI will now delete messages matching the pattern #{fmtTDFA regex}|]

    mkListCommand spec \id_ pat ->
      [i|#{id_}: #{fmtTDFA (pat ^. #regex)}|]

    mkDeleteCommand spec \_id pat ->
      [i|#{fmtTDFA (pat ^. #regex)}|]

    mkReloadCommand spec
