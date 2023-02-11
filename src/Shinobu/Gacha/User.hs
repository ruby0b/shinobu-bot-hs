module Shinobu.Gacha.User where

import Data.Time (Day, UTCTime)
import Database.SQLite.Simple (FromRow (..), field, fromOnly)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Effects.DB
import Shinobu.Gacha.Economy
import Shinobu.Utils.Error
import Shinobu.Utils.Misc

data GachaUser = GachaUser
  { uId :: Word64,
    balance :: Money,
    last_withdrawal :: Maybe UTCTime,
    next_birthday :: Maybe Day,
    mal_username :: Maybe Text,
    is_owner :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromRow GachaUser where
  fromRow = GachaUser <$> field <*> field <*> field <*> field <*> field <*> field

getUser :: DB :> r => Word64 -> P.Sem r (Maybe GachaUser)
getUser uId = query [isql|SELECT * FROM user WHERE id = {uId}|] <&> listToMaybe

getOrCreateUser :: [DB, IntegrityError] :>> r => Word64 -> P.Sem r GachaUser
getOrCreateUser uId = do
  mUser <- getUser uId
  mUser ?! do
    execute [isql|INSERT OR IGNORE INTO user (id) VALUES ({uId})|]
    getUser uId >>?! P.throw @IntegrityErr "couldn't find new user immediately after creation"

allUserIds :: DB :> r => P.Sem r [Word64]
allUserIds = query [isql|SELECT id FROM user|] <&> map fromOnly

addMoney :: DB :> r => Word64 -> Money -> P.Sem r ()
addMoney uId amount = execute [isql|UPDATE user SET balance = balance + {amount} WHERE id = {uId}|]

removeMoney :: DB :> r => Word64 -> Money -> P.Sem r ()
removeMoney uId amount = do
  -- TODO catch negative balance exception and turn into UserError
  execute [isql|UPDATE user SET balance = balance - {amount} WHERE id = {uId}|]
