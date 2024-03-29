module Shinobu.Utils.Types where

import Calamity (BotC)
import Calamity.Commands (DSLC)
import Calamity.Commands.Context (FullContext)
import qualified Polysemy as P
import qualified Polysemy.Conc as P
import qualified Polysemy.Error as P
import qualified Polysemy.Fail as P
import Polysemy.RandomFu (RandomFu)
import qualified Polysemy.Resource as P
import Shinobu.Effects.Cooldown (Cooldown)
import Shinobu.Effects.DB (DB)
import Shinobu.Utils.Error (SomeShinobuException)

type ShinobuC r =
  ( BotC r,
    DSLC FullContext r,
    [ P.Fail,
      P.Resource,
      P.Mask,
      RandomFu,
      Cooldown,
      DB,
      P.Error SomeShinobuException
    ]
      :>> r
  )

type ShinobuSem r = ShinobuC r => P.Sem r ()
