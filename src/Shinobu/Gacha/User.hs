module Shinobu.Gacha.User where

import Data.Time (Day, UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ.Interpolated
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Shinobu.Effects.DB (SQLite, run)
import Shinobu.Effects.IndexStore
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

instance HasKey GachaUser where
  type Key GachaUser = Word64
  getKey = view #uId

type UserStore = IndexStore GachaUser

getUser :: SQLite :> r => Word64 -> P.Sem r (Maybe GachaUser)
getUser uId = run [iquery|SELECT * FROM user WHERE id = ${uId}|] <&> listToMaybe

getOrCreateUser :: [SQLite, IntegrityError] :>> r => Word64 -> P.Sem r GachaUser
getOrCreateUser uId = do
  mUser <- getUser uId
  whenNothing mUser do
    run [iexecute|INSERT OR IGNORE INTO user (id) VALUES (${uId})|]
    getUser uId >>= maybeThrow @IntegrityErr "couldn't find new user immediately after creation"

allUserIds :: SQLite :> r => P.Sem r [Word64]
allUserIds = run [iquery|SELECT id FROM user|] <&> map fromOnly

addMoney :: SQLite :> r => Word64 -> Money -> P.Sem r ()
addMoney uId amount = run [iexecute|UPDATE user SET balance = balance + ${amount} WHERE id = ${uId}|]

removeMoney :: [P.Error Text, SQLite] :>> r => Word64 -> Money -> P.Sem r ()
removeMoney uId amount = do
  -- TODO catch negative balance exception and turn into UserError
  run [iexecute|UPDATE user SET balance = balance - ${amount} WHERE id = ${uId}|]
