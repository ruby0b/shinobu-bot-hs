module Shinobu.Commands.Shop where

import Calamity
import CalamityCommands (Named, command)
import qualified Polysemy.Error as P
import Shinobu.Gacha
import Shinobu.Utils.DB ()
import Shinobu.Utils.Error
import Shinobu.Utils.Misc
import Shinobu.Utils.Types

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

packCmd :: ShinobuSem r
packCmd = void $
  help_ "Buy a pack with the given name. List all currently available packs if given no"
    . command @'[Named "pack name" (Maybe Text)] "pack"
    $ \ctx ->
      tellMyErrors ctx . \case
        Just packName -> void do
          pack <- searchPack packName >>?! P.throw [i|There's no pack named #{packName}!|]
          user <- getOrCreateUser . fromSnowflake . view #id . view #user $ ctx
          embed <- handlePackBuyResult <$> buyPack pack user
          tell ctx embed
        Nothing -> void do
          packs <- allPacksEmbed
          tell ctx packs
