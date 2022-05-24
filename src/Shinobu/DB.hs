{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shinobu.DB where

import Calamity
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.QQ.Interpolated
import Database.SQLite.Simple.ToField (ToField (..))
import Shinobu.Effects.IndexStore (HasKey (..))
import Shinobu.Gacha

deriving newtype instance ToField (Snowflake a)

deriving newtype instance FromField (Snowflake a)

deriving newtype instance ToField Money

deriving newtype instance FromField Money

instance FromRow GachaUser where
  fromRow = GachaUser <$> field <*> field <*> field <*> field

instance FromRow Pack where
  fromRow = Pack <$> field <*> field <*> field <*> field <*> field

-- instance FromRow Rarity where
--   fromRow = Rarity <$> field <*> field

voiceIDToAssociatedTextID :: Snowflake VoiceChannel -> Connection -> IO [Only (Snowflake TextChannel)]
voiceIDToAssociatedTextID voiceID =
  [iquery|
  SELECT voice_id, text_id
  FROM voice_to_text
  WHERE voice_id = ${voiceID}
|]

makeNewUser :: Key GachaUser -> Connection -> IO ()
makeNewUser userID = [iexecute|INSERT OR IGNORE INTO user (id) VALUES (${userID})|]

getBirthdayPeople :: Connection -> IO [GachaUser]
getBirthdayPeople =
  [iquery|
  SELECT *
  FROM user
  WHERE birthday == DATE('now', 'localtime')
|]

rewardAndUpdateBirthday :: Key GachaUser -> Money -> Connection -> IO ()
rewardAndUpdateBirthday userID giftAmount =
  [iexecute|
  UPDATE user
  SET balance = balance + ${giftAmount}
  ,   birthday = DATE(birthday, '+1 years')
  WHERE id = ${userID}
|]

getAvailablePacks :: Connection -> IO [Pack]
getAvailablePacks =
  [iquery|
  SELECT *
  FROM pack
  WHERE (pack.start_date <= DATE('NOW', 'LOCALTIME'))
  AND (pack.end_date IS NULL OR pack.end_date >= DATE('NOW', 'LOCALTIME'))
|]

getAvailablePackByName :: Text -> Connection -> IO [Pack]
getAvailablePackByName packName =
  [iquery|
  SELECT *
  FROM pack
  WHERE (pack.start_date <= DATE('NOW', 'LOCALTIME'))
  AND (pack.end_date IS NULL OR pack.end_date >= DATE('NOW', 'LOCALTIME'))
  AND name LIKE ${packName}
|]

getUserByID :: Key GachaUser -> Connection -> IO [GachaUser]
getUserByID userID = [iquery|SELECT * FROM user WHERE id = ${userID}|]

-- listRarities :: Connection -> IO [Rarity]
-- listRarities = [iquery|SELECT * FROM rarity|]
