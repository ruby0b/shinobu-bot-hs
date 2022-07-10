module Shinobu.Gacha.Economy where

newtype Money = Money {amount :: Int}
  deriving (Show, Generic)
  deriving newtype (Eq, Ord, Num)

currencySymbol :: Text
currencySymbol = "🍩"

showMoney :: Money -> Text
showMoney (Money amount) = show amount <> " " <> currencySymbol
