module Shinobu.Commands.Purge where

import Calamity
import Calamity.Commands
import qualified Control.Foldl as L
import Data.Time (getCurrentTime)
import qualified Polysemy as P
import qualified Polysemy.NonDet as P
import Shinobu.Effects.UserError
import Shinobu.Utils.Calamity
import Shinobu.Utils.Checks
import Shinobu.Utils.Misc
import Shinobu.Utils.Parsers
import Shinobu.Utils.Types
import qualified Streaming.Prelude as S
import Text.RE.TDFA

dryDeleteMessages :: (BotC r, Tellable c) => c -> S.Stream (S.Of Message) (P.Sem r) s -> P.Sem r (Either RestError ())
dryDeleteMessages t msgsStream = do
  msgs <- S.toList_ msgsStream
  void <$> tellInfo t ("This would delete " <> show (length msgs) <> " messages:\n" <> unlines (take 20 (map messageLink msgs)))

purgeCmd :: ShinobuSem r
purgeCmd = void do
  help_ "Bulk delete messages (default limit: 100) (default time span: 1 day)"
    . requiresAdmin
    . commandA
      @'[ Named "limit/time" (Either Int TimeSpan),
          Named "authors" [Snowflake User],
          Named "pattern" (Maybe Text)
        ]
      "purge"
      ["pu"]
    $ \ctx limitOrTime authors pattern_ -> runErrorTellEmbed @RestError ctx $ P.runNonDetMaybe do
      let (limit, timeSpan) = case limitOrTime of
            Left limit' -> (limit', fromTimeSpan $ 24 * 60 * 60)
            Right timeSpan' -> (90, fromTimeSpan timeSpan')
      now <- P.embed getCurrentTime
      regex <- P.embed $ compileRegex $ (toString <$> pattern_) // ".*"
      -- TODO don't use maxBound; always get infinite chunks of 100 or try to be smarter in getManyMessages
      getManyMessages maxBound ctx
        & S.dropWhile ((== getID @Message ctx) . view #id)
        & S.takeWhile (youngerThan @Message timeSpan now)
        & S.filter (\m -> null authors || (m ^. #author & getID @User) `elem` authors)
        & S.filter (matched . (?=~ regex) . view #content)
        & S.take limit
        -- TODO signal end of message history and stop when reached
        & deleteManyMessages ctx
      deleteMessage ctx ctx

spamCmd :: ShinobuSem r
spamCmd = void do
  help_ "Spam messages"
    . requiresAdmin
    . command @'[Named "amount" Int] "spam"
    $ \ctx amount -> void do
      S.replicateM amount (tell ctx "OwO")
        & S.map void
        & delayedChunks 3 1 (L.purely S.fold firstLeft)
