module Shinobu.Commands.Shop where

import Calamity
import CalamityCommands (Named, command, help)
import Data.Sequences (toLower)
import qualified Polysemy as P
import Shinobu.Effects.IndexStore
import Shinobu.Gacha
import Shinobu.Types
import Shinobu.Effects.UserError

handlePackBuyResult :: ForcedWaifuGivingResult -> Embed
handlePackBuyResult = \case
  NewRefunded new _old amount ->
    waifuEmbed new
      & #description ?~ [i|Your new #{rarityName (new ^. #rarity)} got refunded for #{showMoney amount}.|]
  OldRefunded new old amount ->
    waifuEmbed (new ^. #waifu)
      & #description ?~ [i|Your old #{rarityName (old ^. #rarity)} got refunded for #{showMoney amount}.|]
  AutoUpgraded upgraded ->
    waifuEmbed (upgraded ^. #waifu)
      & #description ?~ [i|Your waifu got upgraded to a **#{upgraded ^. #waifu % #rarity}**|]
  NewWaifu new ->
    waifuEmbed (new ^. #waifu)

searchPack :: PackStore :> r => Text -> P.Sem r (Maybe Pack)
searchPack name = listI <&> find \pack -> toLower (pack ^. #name) == toLower name

packCmd :: ShinobuSem r
packCmd = void $
  help (const "Buy a pack with the given name. List all currently available packs if given no")
    . command @'[Named "pack name" (Maybe Text)] "pack"
    $ \ctx -> runUserErrorTellEmbed . \case
      Just packName -> void do
        pack <- searchPack packName >>= maybeUserError ctx [i|There's no pack named #{packName}!|]
        user <- getOrCreateUser . fromSnowflake . view #id . view #user $ ctx
        embed <- handlePackBuyResult <$> buyPack pack user
        tell ctx embed
      Nothing -> void do
        packs <- allPacksEmbed
        tell ctx packs
