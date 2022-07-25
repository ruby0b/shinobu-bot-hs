module Shinobu.Gacha.User where

import Data.Time (Day)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Effects.IndexStore
import Shinobu.Gacha.Economy
import Shinobu.Utils.Misc

data GachaUser = GachaUser
  { uId :: Word64,
    balance :: Money,
    last_withdrawal :: Maybe Day,
    next_birthday :: Maybe Day
  }
  deriving (Show, Eq, Generic)

instance HasKey GachaUser where
  type Key GachaUser = Word64
  getKey = uId

type UserStore = IndexStore GachaUser

newGachaUser :: Word64 -> GachaUser
newGachaUser uId = GachaUser {uId = uId, balance = UnsafeMoney 10, last_withdrawal = Nothing, next_birthday = Nothing}

getOrCreateUser :: UserStore :> r => Word64 -> P.Sem r GachaUser
getOrCreateUser uId = getI uId <&> (// newGachaUser uId)

allUserIds :: UserStore :> r => P.Sem r [Word64]
allUserIds = map (view #uId) <$> listI

addUser :: UserStore :> r => GachaUser -> P.Sem r ()
addUser = putI

addMoney :: UserStore :> r => GachaUser -> Money -> P.Sem r GachaUser
addMoney user amount = do
  let newUser = user & #balance %~ ($+$ amount)
  putI newUser
  return newUser

removeMoney :: [P.Error Text, UserStore] :>> r => GachaUser -> Money -> P.Sem r GachaUser
removeMoney user amount =
  case user ^. #balance $-$ amount of
    Nothing -> P.throw "Not enough money!"
    Just newBalance -> do
      let newUser = user & #balance .~ newBalance
      putI newUser
      return newUser
