module Shinobu.Commands.CustomReactions where

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

data CustomReactionJSON = CustomReactionJSON {regex :: String, response :: Text} deriving (Show, Generic)

makeJSON ''CustomReactionJSON

data CustomReaction = CustomReaction {regex :: RE, response :: Text} deriving (Generic)

instance TryFrom [CustomReactionJSON] [CustomReaction] where
  tryFrom xs = tryFromWrapExc xs $ mapM tryFromElem xs
    where
      tryFromElem j =
        CustomReaction
          <$> compileRegexExc (j ^. #regex)
          <*> pure (j ^. #response)

instance From [CustomReaction] [CustomReactionJSON] where
  from = map \cr -> CustomReactionJSON (reSource (cr ^. #regex)) (cr ^. #response)

customReactions :: ShinobuSem r
customReactions = void
  . runSyncInIO
  . CS.runCachedStateAsJsonFileVia' @[CustomReactionJSON] "data/custom-reactions.json"
  $ do
    react @'MessageCreateEvt
      \(msg, _user, _member) -> do
        reactions <- CS.cached
        let content = msg ^. #content
        forM_ reactions \cr -> do
          let match = content ?=~ (cr ^. #regex)
          let isMatch = matched match
          when isMatch $ void do
            tell msg (cr ^. #response)

    let spec = KeyStoreSpec {groupName = "response", itemSingular = "automatic response", itemPlural = "automatic responses"}

    help_ [i|Manage #{spec ^. #itemPlural}|]
      . requiresAdmin
      . group (spec ^. #groupName)
      $ do
        help_ [i|Add a new #{spec ^. #itemSingular}|]
          . command @'[Named "pattern to match" RegExp, Named "my response" Text] "add"
          $ \ctx (view #regex -> regex) response -> void do
            CS.modify_ (CustomReaction regex response :)
            tellSuccess ctx [i|Understood!\nI will now respond to the pattern #{fmtTDFA regex} by saying:\n#{quote response}|]

        mkListCommand spec \id_ cr ->
          [i|#{id_}: #{fmtTDFA (cr ^. #regex)}\n#{quote (cr ^. #response)}|]

        mkDeleteCommand spec \_id cr ->
          [i|#{fmtTDFA (cr ^. #regex)}\n#{quote (cr ^. #response)}|]

        mkReloadCommand spec
