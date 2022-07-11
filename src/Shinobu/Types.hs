module Shinobu.Types where

import Calamity (BotC)
import Calamity.Commands (DSLC)
import Calamity.Commands.Context (FullContext)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Polysemy.RandomFu (RandomFu)
import Shinobu.Effects.Cooldown (Cooldown)
import Shinobu.Effects.UserError (UserError)
import Shinobu.Gacha.DB (GachaStores)

type ShinobuC r = (BotC r, DSLC FullContext r, GachaStores :>> r, '[RandomFu, P.Error String, Cooldown] :>> r)

type ShinobuSem r = ShinobuC r => P.Sem r ()
