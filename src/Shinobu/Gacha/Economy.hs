module Shinobu.Gacha.Economy where

newtype Money = Money {getMoney :: Int}
  deriving (Show)
  deriving newtype (Eq, Ord, Num)

currencySymbol :: Text
currencySymbol = "🍩"

showMoney :: Money -> Text
showMoney (Money amount) = show amount <> " " <> currencySymbol
