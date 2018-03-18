module Data.StringNumber where

import qualified Data.StringNumber.Helpers as Helpers

newtype StringNumber = StringNumber String
                     deriving (Show, Read)

-- | Translate Integer to String
-- TODO: implement Integer->String translation
numToEnglishString :: Integer -> String
numToEnglishString = undefined

-- | Translate String to Integer
-- TODO: implement String->Integer translation
englishStringToInt :: String -> Integer
englishStringToInt = undefined



-- TODO: make StringNumber instances of Num, Ord, Eq, Enum, Real, and Integral
instance Eq StringNumber where
    (==) = undefined

instance Ord StringNumber where
    compare = undefined

instance Num StringNumber where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Enum StringNumber where
    toEnum = undefined
    fromEnum = undefined

instance Real StringNumber where
    toRational = undefined

instance Integral StringNumber where
    quotRem = undefined
    toInteger = undefined
