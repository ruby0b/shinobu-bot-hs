module Shinobu.Gacha.Embeds where

import Calamity
import Data.Colour.Names (gold)
import qualified Polysemy as P
import Shinobu.Effects.IndexStore
import Shinobu.Gacha.Character
import Shinobu.Gacha.Economy
import Shinobu.Gacha.Pack
import Shinobu.Gacha.Rarity
import Shinobu.Gacha.Waifu

mapToEmbedFields :: Foldable f => (a -> EmbedField) -> f a -> Embed
mapToEmbedFields f xs = foldMap (\x embed -> embed & #fields <>~ [f x]) xs def

charEmbed :: Character -> Embed
charEmbed c =
  def
    & #title ?~ [i|#{c ^. #name} [#{c ^. (#series . #name)}]|]
    & #image .~ (embedImage . from <$> c ^. #image_url)

waifuEmbed :: Waifu -> Embed
waifuEmbed w =
  charEmbed (char w)
    & #color ?~ (w ^. #rarity & rarityColor)
    & #description ?~ [i|**#{w ^. #rarity & rarityName}**|]

allPacksEmbed :: PackStore :> r => P.Sem r Embed
allPacksEmbed = do
  packs <- listI
  return $
    packListEmbed packs
      & #color ?~ gold

packListEmbed :: Foldable f => f Pack -> Embed
packListEmbed packs =
  packs & mapToEmbedFields \pack ->
    let title = [i|#{pack ^. #name} - #{showMoney (pack ^. #cost)} (Available until #{pack ^. #end_date})|]
        description = from $ pack ^. #description
     in embedField title description
