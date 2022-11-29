{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shinobu.Utils.DB where

import Calamity
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField (ToField (..))
import qualified Database.SQLite.Simple.Types as SQL

-- | Wrapper for FromField which shows the field as Text,
-- no matter its actual type (doesn't handle binary blobs)
newtype ShowField = ShowField Text

showField :: ShowField -> Text
showField (ShowField x) = x

data DynShow = forall a. Show a => DynShow a

instance FromField ShowField where
  fromField f =
    ShowField
      . (\case DynShow x -> show x)
      <$> asum
        [ DynShow <$> fromField @SQL.Null f,
          DynShow <$> fromField @Text f,
          DynShow <$> fromField @Integer f,
          DynShow <$> fromField @Double f
        ]

deriving newtype instance ToField (Snowflake a)

deriving newtype instance FromField (Snowflake a)

instance ToField Natural where toField = toField . toInteger

instance FromField Natural where
  fromField f =
    fromField @Integer f >>= \x ->
      if x >= 0
        then return $ fromInteger x
        else returnError ConversionFailed f ("expected a non-negative number but got" ++ show x)

-- voiceIDToAssociatedTextID :: Snowflake VoiceChannel -> Connection -> IO [Only (Snowflake TextChannel)]
-- voiceIDToAssociatedTextID voiceID =
--   [iquery|
--   SELECT voice_id, text_id
--   FROM voice_to_text
--   WHERE voice_id = ${voiceID}

-- | ]

-- makeNewUser :: Key GachaUser -> Connection -> IO ()
-- makeNewUser userID = [iexecute|INSERT OR IGNORE INTO user (id) VALUES (${userID})|]

-- getBirthdayPeople :: Connection -> IO [GachaUser]
-- getBirthdayPeople =
--   [iquery|
--   SELECT *
--   FROM user
--   WHERE birthday == DATE('now', 'localtime')

-- | ]

-- rewardAndUpdateBirthday :: Key GachaUser -> Money -> Connection -> IO ()
-- rewardAndUpdateBirthday userID giftAmount =
--   [iexecute|
--   UPDATE user
--   SET balance = balance + ${giftAmount}
--   ,   birthday = DATE(birthday, '+1 years')
--   WHERE id = ${userID}

-- | ]

-- getAvailablePacks :: Connection -> IO [Pack]
-- getAvailablePacks =
--   [iquery|
--   SELECT *
--   FROM pack
--   WHERE (pack.start_date <= DATE('NOW', 'LOCALTIME'))
--   AND (pack.end_date IS NULL OR pack.end_date >= DATE('NOW', 'LOCALTIME'))

-- | ]

-- getAvailablePackByName :: Text -> Connection -> IO [Pack]
-- getAvailablePackByName packName =
--   [iquery|
--   SELECT *
--   FROM pack
--   WHERE (pack.start_date <= DATE('NOW', 'LOCALTIME'))
--   AND (pack.end_date IS NULL OR pack.end_date >= DATE('NOW', 'LOCALTIME'))
--   AND name LIKE ${packName}

-- | ]

-- getUserByID :: Key GachaUser -> Connection -> IO [GachaUser]
-- getUserByID userID = [iquery|SELECT * FROM user WHERE id = ${userID}|]

-- listRarities :: Connection -> IO [Rarity]
-- listRarities = [iquery|SELECT * FROM rarity|]
