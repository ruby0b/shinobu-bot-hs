module Shinobu.Commands.Misc where

import Calamity
import Calamity.Commands
import Data.Random (stdUniform)
import qualified Polysemy.RandomFu as P
import Shinobu.Gacha.User (allUserIds, getOrCreateUser)
import Shinobu.Utils.Error
import Shinobu.Utils.Misc
import Shinobu.Utils.Types

miscCommands :: ShinobuSem r
miscCommands = void do
  help_ "Blame someone"
    . command @'[Named "user" (Maybe User)] "blame"
    $ \ctx maybeU -> void do
      let blamed = maybeU // (ctx ^. #user)
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

  help_ "list all users"
    . command @'[] "list-users"
    $ \ctx -> void do
      ids <- allUserIds
      if null ids
        then tellInfo ctx "No users!"
        else tellInfo ctx . unlines . map show $ ids

  help_ "add a user"
    . command @'[Named "user" User] "add-user"
    $ \ctx user -> tellMyErrors ctx do
      getOrCreateUser . fromSnowflake . view #id $ user
      tellInfo ctx [i|Successfully added #{mention user} ✔️|]

  help_ "beep boop"
    . command @'[] "fail"
    $ const $ void $ fail "ahhhhh"
