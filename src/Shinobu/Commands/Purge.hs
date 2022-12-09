module Shinobu.Commands.Purge where

import Calamity
import Calamity.Commands
import qualified Control.Foldl as L
import Data.Time (getCurrentTime)
import qualified Polysemy as P
import qualified Polysemy.NonDet as P
import Shinobu.Utils.Calamity
import Shinobu.Utils.Checks
import Shinobu.Utils.Error
import Shinobu.Utils.Misc
import Shinobu.Utils.Parsers
import qualified Shinobu.Utils.Streaming as S
import Shinobu.Utils.Types
import qualified Streaming.Prelude as S
import Text.RE.TDFA

dryDeleteMessages :: (BotC r, Tellable c) => c -> S.Stream (S.Of Message) (P.Sem r) s -> P.Sem r (Either RestError ())
dryDeleteMessages t msgsStream = do
  msgs <- S.toList_ msgsStream
  void <$> tellInfo t ("This would delete " <> show (length msgs) <> " messages:\n" <> unlines (take 20 (map messageLink msgs)))

purgeCmd :: ShinobuSem r
purgeCmd = void do
  help_ "Bulk delete messages (default limit: 100 messages and 1 week)"
    . requiresAdmin
    . commandA
      @'[ Named "limit" (Either TimeSpan Int),
          Named "authors" [Snowflake User],
          Named "pattern" (Maybe RegExp)
        ]
      "purge"
      ["pu"]
    $ \ctx timeOrLimit authors (fmap (view #regex) -> mRegex) ->
      tellMyErrors ctx $ P.runNonDetMaybe do
        let (limit, timeSpan) = case timeOrLimit of
              Left timeSpan' -> (100, timeSpan' ^. #diffTime)
              Right limit' -> (limit', (7 * 24 * 60 * 60 :: TimeSpan) ^. #diffTime)
        now <- P.embed getCurrentTime
        let regex = mRegex // [re|.*|]
        let estimate =
              if null authors && isNothing mRegex
                then Just (min 100 (limit + 1))
                else Nothing
        getManyMessages estimate ctx
          & S.takeWhile (youngerThan @Message timeSpan now)
          & S.dropWhile ((== getID @Message ctx) . view #id)
          & S.filter (\m -> null authors || (m ^. #author & getID @User) `elem` authors)
          & S.filter (matched . (?=~ regex) . view #content)
          & S.take limit
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
        & delayedChunks 3 1 (L.purely S.fold S.firstLeft)
