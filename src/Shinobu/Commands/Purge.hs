module Shinobu.Commands.Purge where

import Calamity
import Calamity.Commands
import Data.Time (getCurrentTime)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.NonDet as P
import Shinobu.Effects.UserError
import Shinobu.Utils.Calamity
import Shinobu.Utils.Checks
import Shinobu.Utils.Misc
import Shinobu.Utils.Parsers
import Shinobu.Utils.Types
import Text.RE.TDFA

dryDeleteMessages :: (BotC r, Tellable c) => c -> [Message] -> P.Sem r (Either RestError ())
dryDeleteMessages t msgs =
  void <$> tellInfo t ("This would delete " <> show (length msgs) <> " messages:\n" <> unlines (take 20 $ map messageLink msgs))

purgeCmd :: ShinobuSem r
purgeCmd = void do
  help_ "Bulk delete messages (limit: 100) (default time span: 10 minutes)"
    . requiresAdmin
    . commandA
      @'[ Named "time span" (Maybe TimeSpan),
          Named "authors" [Snowflake User],
          Named "pattern" (Maybe Text)
        ]
      "purge"
      ["pu"]
    $ \ctx timeSpanM authors pattern_ -> runErrorTellEmbed @RestError ctx $ P.runNonDetMaybe do
      when (isNothing timeSpanM && null authors && null pattern_) do
        tellError ctx "You have to specify at least one parameter to prevent accidental purges!"
        empty
      let limit = 100
      let timeSpan = fromTimeSpan $ timeSpanM // 600
      now <- P.embed getCurrentTime
      regex <- P.embed $ compileRegex $ (toString <$> pattern_) // ".*"
      msgs <- P.fromEither =<< invoke (GetChannelMessages ctx Nothing (Just $ ChannelMessagesLimit limit))
      deleteManyMessages ctx $
        msgs
          & filter ((/= getID @Message ctx) . view #id)
          & filter (youngerThan @Message timeSpan now)
          & filter (\m -> null authors || (m ^. #author & getID @User) `elem` authors)
          & filter (matched . (?=~ regex) . view #content)
      deleteMessage ctx ctx
