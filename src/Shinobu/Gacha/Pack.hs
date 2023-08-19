module Shinobu.Gacha.Pack where

import Data.Random.List (randomElement)
import Data.Text (toLower)
import Data.Time (Day)
import Database.SQLite.Simple (FromRow (..), field)
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import Polysemy.RandomFu qualified as P
import Shinobu.Effects.DB
import Shinobu.Gacha.Character
import Shinobu.Gacha.Economy
import Shinobu.Gacha.Rarity
import Shinobu.Gacha.User
import Shinobu.Gacha.Waifu
import Shinobu.Utils.Error

data Pack = Pack
  { name :: Text,
    cost :: Money,
    description :: Text,
    start_date :: Day,
    end_date :: Maybe Day
  }
  deriving (Show, Generic)

instance FromRow Pack where
  fromRow = Pack <$> field <*> field <*> field <*> field <*> field

listPacks :: P.Member DB r => P.Sem r [Pack]
listPacks = query [isql|SELECT * FROM pack|]

searchPack :: DB :> r => Text -> P.Sem r (Maybe Pack)
searchPack name =
  query [isql|SELECT * FROM pack WHERE name LIKE {toLower name}|]
    <&> listToMaybe

buyPack :: [P.Fail, UserError, P.RandomFu, DB] :>> r => Pack -> GachaUser -> P.Sem r ForcedWaifuGivingResult
buyPack pack buyer = do
  removeMoney (buyer ^. #uId) (pack ^. #cost) & intoUserError
  waifu <- sampleWaifu pack
  forceGiveWaifu buyer waifu

sampleWaifu :: P.RandomFu :> r => Pack -> P.Sem r Waifu
sampleWaifu pack = do
  rarity <- do
    rarityT <- sampleRarityType
    return $ Rarity rarityT Basic
  character <- sampleCharacter pack rarity
  return (Waifu character rarity)

sampleCharacter :: P.RandomFu :> r => Pack -> Rarity -> P.Sem r Character
sampleCharacter _pack _rarity = P.sampleRVar $ randomElement allChars
