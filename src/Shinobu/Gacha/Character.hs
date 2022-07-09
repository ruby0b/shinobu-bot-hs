module Shinobu.Gacha.Character where

import Shinobu.Effects.IndexStore

newtype Series = Series {name :: Text}
  deriving (Show, Eq, Generic)

newtype Batch = Batch {name :: Text}
  deriving (Show, Eq, Generic)

data Character = Character
  { id :: Integer,
    name :: Text,
    image_url :: Maybe Text,
    batch :: Batch,
    series :: Series
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''Series
makeFieldLabelsNoPrefix ''Batch
makeFieldLabelsNoPrefix ''Character

instance HasKey Character where
  type Key Character = Integer
  getKey = view #id

type CharacterStore = IndexStore Character

allChars =
  [ Character
      { id = 1,
        name = "Chane Laforet",
        image_url = Just "https://media.discordapp.net/attachments/270546463023431692/822534314502783036/e574b8784dfda625919e417cc254926a.png?width=459&height=649",
        batch = Batch "classic",
        series = Series "Baccano!"
      },
    Character
      { id = 2,
        name = "Holo",
        image_url = Just "https://media.discordapp.net/attachments/270546463023431692/662396858429341707/37be6fb4bbbbd39ef298da8b86d25d89.png?width=765&height=1170",
        batch = Batch "classic",
        series = Series "Ookami to Koushinryou"
      }
  ]
