module Shinobu.Utils.JSON where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import Data.Char (isUpper, toLower)
import Data.List.Split (keepDelimsL, split, whenElt)
import qualified Polysemy as P
import qualified Polysemy.Error as P

newtype FromJSONException = FromJSONException String
  deriving stock (Show)
  deriving anyclass (Exception)

makeJSON =
  JSON.deriveJSON
    JSON.defaultOptions
      { JSON.fieldLabelModifier = toKebabCase
      }

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = error "Cannot use 'mapHead' on empty List"
mapHead f (x : xs) = f x : xs

toKebabCase :: String -> String
toKebabCase =
  intercalate "-"
    . map (mapHead toLower)
    . split (keepDelimsL $ whenElt isUpper)

decodeVia ::
  forall a b r.
  [P.Error FromJSONException, P.Error (TryFromException a b)] :>> r =>
  (JSON.FromJSON a, TryFrom a b) =>
  ByteString ->
  P.Sem r b
decodeVia =
  JSON.eitherDecodeStrict @a
    >>> first FromJSONException
    >>> P.fromEither
    >=> tryInto @b
    >>> P.fromEither

encodeVia :: forall a b. (JSON.ToJSON a, From b a) => b -> ByteString
encodeVia = toStrict . JSON.encode @a . from @b
