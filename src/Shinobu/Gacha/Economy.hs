module Shinobu.Gacha.Economy where

newtype Money = Money {amount :: Int}
  deriving (Show)
  deriving newtype (Eq, Ord, Num)

makeFieldLabelsNoPrefix ''Money

currencySymbol :: Text
currencySymbol = "🍩"

showMoney :: Money -> Text
showMoney (Money amount) = show amount <> " " <> currencySymbol
