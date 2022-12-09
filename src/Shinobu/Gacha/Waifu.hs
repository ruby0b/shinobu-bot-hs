module Shinobu.Gacha.Waifu where

import qualified Polysemy as P
import qualified Polysemy.Fail as P
import Shinobu.Effects.IndexStore
import Shinobu.Gacha.Character
import Shinobu.Gacha.Economy
import Shinobu.Gacha.Rarity
import Shinobu.Gacha.User
import Shinobu.Utils.Error
import Shinobu.Utils.Misc

data Waifu = Waifu
  { char :: Character,
    rarity :: Rarity
  }
  deriving (Show, Eq, Generic)

data OwnedWaifu = OwnedWaifu
  { waifu :: Waifu,
    owner :: GachaUser
  }
  deriving (Show, Eq, Generic)

instance HasKey OwnedWaifu where
  type Key OwnedWaifu = (Key GachaUser, Key Character)
  getKey ow = (ow ^. #owner % to getKey, ow ^. #waifu % #char % to getKey)

type WaifuStore = IndexStore OwnedWaifu

data ForcedWaifuGivingResult
  = NewRefunded Waifu OwnedWaifu Money
  | OldRefunded OwnedWaifu Waifu Money
  | AutoUpgraded OwnedWaifu
  | NewWaifu OwnedWaifu

forceGiveWaifu :: [P.Fail, UserError] :>> r => GachaUser -> Waifu -> P.Sem r ForcedWaifuGivingResult
forceGiveWaifu user newWaifu =
  giveWaifu user newWaifu >>= \case
    Duplicate oldWaifu cmp ->
      case cmp of
        LT -> do
          (refundAmount, refunded) <- unsafeRefund user newWaifu
          pure $ NewRefunded refunded oldWaifu refundAmount
        EQ -> do
          (upgraded, _cost) <- unsafeAutoUpgrade oldWaifu
          pure $ AutoUpgraded upgraded
        GT -> do
          (refundAmount, refunded) <- refund user oldWaifu
          newOwnedWaifu <- unsafeGiveWaifu user newWaifu
          pure $ OldRefunded newOwnedWaifu refunded refundAmount
    Unique newOwnedWaifu -> do
      pure $ NewWaifu newOwnedWaifu

data WaifuGivingResult
  = -- | Duplicate (the old waifu) (new rarity compared to old rarity)
    Duplicate OwnedWaifu Ordering
  | -- | Unique (the new waifu which the user did not own before)
    Unique OwnedWaifu

giveWaifu :: GachaUser -> Waifu -> P.Sem r WaifuGivingResult
giveWaifu user newWaifu = do
  let waifuName = newWaifu ^. #char % #name
  getWaifuByName user waifuName >>= \case
    Just oldWaifu -> do
      let cmp = compare (newWaifu ^. #rarity) (oldWaifu ^. #waifu % #rarity)
      pure $ Duplicate oldWaifu cmp
    Nothing -> do
      newOwnedWaifu <- unsafeGiveWaifu user newWaifu
      pure $ Unique newOwnedWaifu

refund :: GachaUser -> OwnedWaifu -> P.Sem r (Money, Waifu)
refund user ownedWaifu = unsafeRefund user (ownedWaifu ^. #waifu)

-- TODO: using linear types to ensure payment would give us a safe upgrade
unsafeAutoUpgrade :: P.Fail :> r => OwnedWaifu -> P.Sem r (OwnedWaifu, Money)
unsafeAutoUpgrade ownedWaifu =
  let rarity = ownedWaifu ^. #waifu % #rarity
      user = ownedWaifu ^. #owner
      invalidRarity = [i|Unable to upgrade rarity #{rarity}|]
   in case rarity of
        Rarity t Plus -> do
          (newRarity, cost) <- maybe (fail invalidRarity) return do
            cost <- upgradeCost rarity
            rarityT <- maybeSucc t
            Just (Rarity rarityT Basic, cost)

          let newWaifu =
                ownedWaifu ^. #waifu
                  & #rarity .~ newRarity

          -- TODO open DB transaction
          removeWaifu ownedWaifu
          upgradedWaifu <- unsafeGiveWaifu user newWaifu
          -- close DB transaction

          pure (upgradedWaifu, cost)
        Rarity _ Basic -> fail invalidRarity

removeWaifu :: OwnedWaifu -> P.Sem r ()
removeWaifu = pure (pure ()) -- TODO

unsafeGiveWaifu :: GachaUser -> Waifu -> P.Sem r OwnedWaifu
unsafeGiveWaifu user waifu = pure (OwnedWaifu {owner = user, waifu = waifu}) -- TODO

unsafeRefund :: GachaUser -> Waifu -> P.Sem r (Money, Waifu)
unsafeRefund _user waifu = pure (refundValue (waifu ^. #rarity), waifu) -- TODO

getWaifuByName :: GachaUser -> Text -> P.Sem r (Maybe OwnedWaifu)
getWaifuByName _user _waifuName = pure Nothing -- TODO
