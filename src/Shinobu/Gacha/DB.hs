module Shinobu.Gacha.DB where

import qualified Polysemy as P
import Shinobu.Effects.IndexStore (runIndexStoreAsKeyStore, toKeyMap)
import Shinobu.Effects.KeyStore (runKeyStoreIORef)
import Shinobu.Gacha.Character (CharacterStore, allChars)
import Shinobu.Gacha.Pack (PackStore, allPacks)
import Shinobu.Gacha.User (UserStore)
import Shinobu.Gacha.Waifu (WaifuStore)

type GachaStores = '[UserStore, PackStore, CharacterStore, WaifuStore]

runGachaStoresIO ::
  P.Embed IO :> r =>
  P.Sem (GachaStores ++ r) a ->
  P.Sem r a
runGachaStoresIO sem = do
  uRef <- P.embed $ newIORef mempty
  pRef <- P.embed $ newIORef $ toKeyMap allPacks
  cRef <- P.embed $ newIORef $ toKeyMap allChars
  wRef <- P.embed $ newIORef mempty
  let run ref = runKeyStoreIORef ref . runIndexStoreAsKeyStore
  run wRef . run cRef . run pRef . run uRef $ sem
