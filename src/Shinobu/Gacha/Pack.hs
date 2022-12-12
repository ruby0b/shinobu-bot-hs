module Shinobu.Gacha.Pack where

import Data.Random.List (randomElement)
import Data.Text (toLower)
import Data.Time (Day)
import Data.Time.Calendar.Julian (fromJulian)
import Database.SQLite.Simple (FromRow (..), field)
import Database.SQLite.Simple.QQ.Interpolated
import qualified Polysemy as P
import qualified Polysemy.Fail as P
import qualified Polysemy.RandomFu as P
import Shinobu.Effects.DB
import Shinobu.Effects.IndexStore
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

instance HasKey Pack where
  type Key Pack = Text
  getKey = view #name

type PackStore = IndexStore Pack

allPacks =
  [ Pack
      { name = "classic",
        cost = UnsafeMoney 10,
        description = "sample text idk",
        start_date = fromJulian 2020 10 5,
        end_date = Just $ fromJulian 2021 05 30
      }
  ]

searchPack :: DB :> r => Text -> P.Sem r (Maybe Pack)
searchPack name =
  query [isql|SELECT * FROM pack WHERE name LIKE ${toLower name}|]
    <&> listToMaybe

buyPack :: [P.Fail, UserError, P.RandomFu, UserStore, DB] :>> r => Pack -> GachaUser -> P.Sem r ForcedWaifuGivingResult
buyPack pack buyer = do
  removeMoney (buyer ^. #uId) (pack ^. #cost) & intoUserError
  waifu <- sampleWaifu pack
  forceGiveWaifu buyer waifu

sampleWaifu :: P.RandomFu :> r => Pack -> P.Sem r Waifu
sampleWaifu pack = do
  rarity <- sampleBasic
  character <- sampleCharacter pack rarity
  return (Waifu character rarity)

sampleCharacter :: P.RandomFu :> r => Pack -> Rarity -> P.Sem r Character
sampleCharacter _pack _rarity = P.sampleRVar $ randomElement allChars
