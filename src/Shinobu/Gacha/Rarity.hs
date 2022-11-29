module Shinobu.Gacha.Rarity where

import Data.Colour (Colour)
import Data.Colour.Names (cyan, gold, grey, red)
import qualified Data.Random.Distribution.Categorical as C
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import qualified Polysemy as P
import qualified Polysemy.RandomFu as P
import Relude.Extra (safeToEnum, typeName)
import Shinobu.Gacha.Economy

-- TODO rework the upgrade system entirely
data RarityType = Common | Rare | Legendary | Godlike
  deriving (Show, Eq, Ord, Enum, Bounded)

data RarityVal = Basic | Plus
  deriving (Show, Eq, Ord, Enum, Bounded)

data Rarity = Rarity RarityType RarityVal
  deriving (Show, Eq, Ord)

fromFieldToBoundedEnum :: forall t. (Bounded t, Enum t, Typeable t) => Field -> Ok t
fromFieldToBoundedEnum f =
  fromField @Int f <&> safeToEnum >>= \case
    Nothing -> returnError ConversionFailed f (toString (typeName @t) ++ " enum value out of range")
    Just rt -> pure rt

instance FromField RarityType where
  fromField = fromFieldToBoundedEnum

instance FromField RarityVal where
  fromField = fromFieldToBoundedEnum

instance FromRow Rarity where
  fromRow = Rarity <$> field <*> field

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
refundValue (Rarity r Basic) = UnsafeMoney $ case r of
  Common -> 3
  Rare -> 10
  Legendary -> 50
  Godlike -> 200
refundValue (Rarity r Plus) = UnsafeMoney 2 $*$ refundValue (Rarity r Basic)

upgradeCost :: Rarity -> Maybe Money
upgradeCost (Rarity _ Basic) = Nothing
upgradeCost (Rarity t Plus) = case t of
  Common -> Just $ UnsafeMoney 10
  Rare -> Just $ UnsafeMoney 50
  Legendary -> Just $ UnsafeMoney 100
  _ -> Nothing

rarityDist :: C.Categorical Double RarityType
rarityDist = C.fromList [(125, Common), (25, Rare), (5, Legendary), (1, Godlike)]

sampleBasic :: P.RandomFu :> r => P.Sem r Rarity
sampleBasic = do
  rarityT <- P.sampleDist rarityDist
  return (Rarity rarityT Basic)
