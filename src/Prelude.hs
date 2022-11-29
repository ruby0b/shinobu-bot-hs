module Prelude
  ( module Optics,
    module Optics.TH,
    module Data.Default,
    module Data.String.Interpolate,
    module Relude,
    module Witch,
    type (:>),
    type (:>>),
    type (++),
    (<.>),
  )
where

import Data.Default (def)
import Data.String.Interpolate (i)
import Optics (to, view, (%), (%?), (%~), (.~), (?~), (^.))
import Optics.TH
import qualified Polysemy as P
import Relude hiding (group)
import Witch (From (from), TryFrom (tryFrom), TryFromException (..), into, tryInto)

default (Text)

-- type operators for effects, taken from cleff
type x :> r = P.Member x r

type xs :>> r = P.Members xs r

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
(<.>) f g x = f <$> g x

infixl 4 <.>
