module Shinobu.Commands.Misc where

import Calamity
import Calamity.Commands
import Data.Random (stdUniform)
import qualified Polysemy.RandomFu as P
import Shinobu.Utils.Misc
import Shinobu.Utils.Types

miscCommands :: ShinobuSem r
miscCommands = void do
  help_ "Blame someone"
    . command @'[Named "user" (Maybe User)] "blame"
    $ \ctx maybeU -> void do
      let blamed = maybeU ?: (ctx ^. #user)
      let response :: Text = [i|Blame #{mention blamed} for everything!|]
      tell ctx response

  help_ "Flip a coin!"
    . command @'[] "flip"
    $ \ctx -> void do
      rand <- P.sampleRVar $ stdUniform @Double
      tellInfo
        ctx
        if rand < 0.5
          then "Heads!"
          else "Tails!"

  help_ "Get a link to the source code of this bot"
    . command @'[] "source"
    $ \ctx -> void do
      tellInfo ctx "The source code for this bot is available at https://github.com/ruby0b/shinobu-bot-hs"

  help_ "beep boop"
    . command @'[] "fail"
    $ const $ void $ fail "ahhhhh"
