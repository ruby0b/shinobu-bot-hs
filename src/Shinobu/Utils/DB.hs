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
