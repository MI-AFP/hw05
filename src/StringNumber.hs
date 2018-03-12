module StringNumber where

-- TODO: make it instance of Num (and other necessary)
newtype StringNumber = StringNumber String deriving (Show, Read)

-- | Translate Integer to String
-- TODO: implement Integer->String translation
numToEnglishString :: Integer -> String
numToEnglishString = undefined

-- | Translate String to Integer
-- TODO: implement String->Integer translation
englishStringToInt :: String -> Integer
englishStringToInt = undefined
