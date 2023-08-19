{-# OPTIONS_GHC -Wno-orphans #-}

module Shinobu.Commands.CustomReactions where

import Calamity
import Calamity.Commands
import qualified Polysemy.Error as P
import Shinobu.Effects.Cache
import Shinobu.Effects.DB
import Shinobu.Utils.Checks
import Shinobu.Utils.Error
import Shinobu.Utils.KeyStoreCommands
import Shinobu.Utils.Misc
import Shinobu.Utils.Parsers
import Shinobu.Utils.Types
import Text.RE.TDFA

table = "regex_reactions"

spec =
  KeyStoreSpec
    { tableName = table,
      groupName = "response",
      itemSingular = "automatic response",
      itemPlural = "automatic responses"
    }

type CR_SQL = (Integer, String, Text)

type CR = (Integer, RE, Text)

instance P.Error RegexCompilationException :> r => TryFromP CR_SQL CR r where
  tryFromP (id_, rawRE, response) = do
    regex <- compileRegexErr rawRE
    return (id_, regex, response)

customReactions :: ShinobuSem r
customReactions = void
  . runSyncInIO
  . intoSomeShinobuException @RegexCompilationException
  . evalCacheViaState @[CR] (queryVia @CR_SQL [isql|SELECT * FROM !{table}|])
  $ do
    react @'MessageCreateEvt
      \(msg, _user, _member) -> do
        reactions <- cached
        let content = msg ^. #content
        forM_ reactions \(_id, pattern_, response) -> do
          let match = content ?=~ pattern_
          let isMatch = matched match
          when isMatch $ void do
            tell msg response

    help_ [i|Manage #{spec ^. #itemPlural}|]
      . requiresAdmin
      . group (spec ^. #groupName)
      $ do
        help_ [i|Add a new #{spec ^. #itemSingular}|]
          . command @'[Named "pattern to match" RegExp, Named "my response" Text] "add"
          $ \ctx (view #regex -> regex) response -> tellMyErrors ctx do
            execute [isql|INSERT INTO !{table} (regex, reaction) VALUES ({reSource regex}, {response})|]
            tellSuccess ctx [i|Understood!\nI will now respond to the pattern #{fmtTDFA regex} by saying:\n#{quote response}|]

        mkListCommand spec \(id_, pattern_, response) ->
          [i|#{id_}: #{fmtTDFA pattern_}\n#{quote response}|]

        mkDeleteCommand @Integer @CR_SQL spec \(_id, pattern_, response) ->
          [i|#{fmtTDFA pattern_}\n#{quote response}|]

        mkReloadCommand spec
