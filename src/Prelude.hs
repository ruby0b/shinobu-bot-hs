module Prelude
  ( module Control.Lens,
    module Control.Lens.Operators,
    module Data.Default,
    module Data.Generics.Labels,
    module Data.String.Interpolate,
    module Relude,
    module Witch,
    type (:>),
    type (:>>),
    type (++),
  )
where

import Control.Lens (to, view)
import Control.Lens.Operators ((+~), (.~), (<>~), (?~), (^.))
import Data.Default (def)
import Data.Generics.Labels -- orphan instances
import Data.String.Interpolate (i)
import qualified Polysemy as P
import Relude hiding (group)
import Witch (from)

default (Text)

-- type operators for effects, taken from cleff
type x :> r = P.Member x r

type xs :>> r = P.Members xs r

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++
