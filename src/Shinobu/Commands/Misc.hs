module Shinobu.Commands.Misc where

import Calamity (Mentionable (mention), Snowflake (fromSnowflake), tell)
import Calamity.Commands (Named, command, help)
import Calamity.Types (User)
import Data.Random (stdUniform)
import qualified Polysemy.RandomFu as P
import Shinobu.Gacha.User (allUserIds, getOrCreateUser)
import Shinobu.Types
import Shinobu.Util

miscCommands :: ShinobuSem r
miscCommands = void do
  help (const "Blame someone")
    . command @'[Named "user" (Maybe User)] "blame"
    $ \ctx maybeU -> void do
      let blamed = maybeU // (ctx ^. #user)
      let response :: Text = [i|Blame #{mention blamed} for everything!|]
      tell ctx response

  help (const "Flip a coin!")
    . command @'[] "flip"
    $ \ctx -> void do
      rand <- P.sampleRVar $ stdUniform @Double
      tell
        ctx
        if rand < 0.5
          then "Heads!"
          else "Tails!"

  help (const "Get a link to the source code of this bot")
    . command @'[] "source"
    $ \ctx -> void do
      tell ctx "The source code for this bot is available at https://github.com/ruby0b/shinobu-bot-hs"

  help (const "list all users")
    . command @'[] "list-users"
    $ \ctx -> void do
      ids <- allUserIds
      forM ids $ tell ctx . show

  help (const "add a user")
    . command @'[Named "user" User] "add-user"
    $ \ctx user -> void do
      getOrCreateUser . fromSnowflake . view #id $ user
      tell @Text ctx [i|Successfully added #{mention user} ✔️|]
