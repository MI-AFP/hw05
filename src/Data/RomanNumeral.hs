module Data.RomanNumeral where

import Data.Maybe (fromMaybe)

-- Use Data.RomanNumeral.Helpers submodule
-- In case of need, feel free to change or enhance Helpers or create own
-- submodule
--
-- DO NOT HARDCODE ANY STRINGs/CHARs IN THIS MODULE!
import qualified Data.RomanNumeral.Helpers as Helpers

-- | RomanNumeral type (wrapper) for English numerals
newtype RomanNumeral = RomanNumeral String
                     deriving (Show, Read)

   -- | Pack Integer into RomanNumeral (English numeral string)
pack :: (Integral a, Show a) => a -> RomanNumeral
pack integral = RomanNumeral $ fromMaybe err (integral2RomanNumeral integral)
              where err = error $ Helpers.messageBadIntegral integral

-- | Unpack RomanNumeral (English numeral string) to Integer
unpack :: RomanNumeral -> Integer
unpack (RomanNumeral numeral) = fromMaybe err (romanNumeral2Integral numeral)
                              where err = error $ Helpers.messageBadNumeral numeral


-- | Translate Integral value to Roman Numeral String (if possible)
-- TODO: implement Integral->String translation
integral2RomanNumeral :: (Integral a, Show a) => a -> Maybe String
integral2RomanNumeral = undefined

-- | Translate Roman Numeral String to Integral value (if possible)
-- TODO: implement String->Integral translation
romanNumeral2Integral :: (Integral a, Show a) => String -> Maybe a
romanNumeral2Integral = undefined

-- TODO: implement RomanNumeral instances of Bounde, Num, Ord, Eq, Enum, Real, and Integral
instance Bounded RomanNumeral where
    minBound = undefined
    maxBound = undefined

instance Eq RomanNumeral where
    (==) = undefined

instance Ord RomanNumeral where
    compare = undefined

instance Num RomanNumeral where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Enum RomanNumeral where
    toEnum = undefined
    fromEnum = undefined

instance Real RomanNumeral where
    toRational = undefined

instance Integral RomanNumeral where
    quotRem = undefined
    toInteger = undefined
