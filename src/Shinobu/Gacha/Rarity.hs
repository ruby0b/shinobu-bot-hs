module Shinobu.Gacha.Rarity where

import Data.Colour (Colour)
import Data.Colour.Names (cyan, gold, grey, red)
import qualified Data.Random.Distribution.Categorical as C
import qualified Polysemy as P
import qualified Polysemy.RandomFu as P
import Shinobu.Gacha.Economy (Money)

data RarityType = Common | Rare | Legendary | Godlike
  deriving (Show, Eq, Ord, Enum, Bounded)

data RarityVal = Basic | Plus
  deriving (Show, Eq, Ord, Enum, Bounded)

data Rarity = Rarity RarityType RarityVal
  deriving (Show, Eq, Ord)

rarityColor :: Rarity -> Colour Double
rarityColor (Rarity t _) = case t of
  Common -> grey
  Rare -> cyan
  Legendary -> gold
  Godlike -> red

rarityName :: Rarity -> Text
rarityName (Rarity t Basic) = show t
rarityName (Rarity t Plus) = rarityName (Rarity t Basic) <> "+"

refundValue :: Rarity -> Money
refundValue (Rarity r Basic) = case r of
  Common -> 3
  Rare -> 10
  Legendary -> 50
  Godlike -> 200
refundValue (Rarity r Plus) = 2 * refundValue (Rarity r Basic)

upgradeCost :: Rarity -> Maybe Money
upgradeCost (Rarity _ Basic) = Nothing
upgradeCost (Rarity t Plus) = case t of
  Common -> Just 10
  Rare -> Just 50
  Legendary -> Just 100
  _ -> Nothing

rarityDist :: C.Categorical Double RarityType
rarityDist = C.fromList [(125, Common), (25, Rare), (5, Legendary), (1, Godlike)]

sampleBasic :: P.RandomFu :> r => P.Sem r Rarity
sampleBasic = do
  rarityT <- P.sampleDist rarityDist
  return (Rarity rarityT Basic)
