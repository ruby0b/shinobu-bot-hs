module Shinobu.Gacha.Economy where

import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Shinobu.Utils.DB ()

newtype Money = UnsafeMoney {amount :: Natural}
  deriving (Show, Generic)
  deriving newtype (Eq, Ord)

deriving newtype instance ToField Money

deriving newtype instance FromField Money

($+$) :: Money -> Money -> Money
(UnsafeMoney x) $+$ (UnsafeMoney y) = UnsafeMoney (x + y)

infixl 6 $+$

($-$) :: Money -> Money -> Maybe Money
(UnsafeMoney x) $-$ (UnsafeMoney y)
  | x < y = Nothing
  | otherwise = Just $ UnsafeMoney (x - y)

infixl 6 $-$

($*$) :: Money -> Money -> Money
(UnsafeMoney x) $*$ (UnsafeMoney y) = UnsafeMoney (x * y)

infixl 7 $*$

currencySymbol :: Text
currencySymbol = "🍩"

showMoney :: Money -> Text
showMoney (UnsafeMoney amount) = show amount <> " " <> currencySymbol
