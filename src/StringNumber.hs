module StringNumber where

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



-- TODO: make String instance of Num, Ord, Eq
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


------------------------------------------------------
lowNumbers  0 = "zero"
lowNumbers  1 = "one"
lowNumbers  2 = "two"
lowNumbers  3 = "three"
lowNumbers  4 = "four"
lowNumbers  5 = "five"
lowNumbers  6 = "six"
lowNumbers  7 = "seven"
lowNumbers  8 = "eight"
lowNumbers  9 = "nine"
lowNumbers 10 = "ten"
lowNumbers 11 = "eleven" -- See: https://www.youtube.com/watch?v=NMS2VnDveP8
lowNumbers 12 = "twelve"
lowNumbers 13 = "thirteen"
lowNumbers 14 = "fourteen"
lowNumbers 15 = "fifteen"
lowNumbers 16 = "sixteen"
lowNumbers 17 = "seventeen"
lowNumbers 18 = "eighteen"
lowNumbers 19 = "nineteen"
lowNumbers  x = error "Now a low number: " ++ show x


-- 10 * x (ten to ninety)
tens 1 = "ten"
tens 2 = "twenty"
tens 3 = "thirty"
tens 4 = "forty"
tens 5 = "fifty"
tens 6 = "sixty"
tens 7 = "seventy"
tens 8 = "eighty"
tens 9 = "ninety"
tens x = error "Undefined ten: " ++ show x

-- 10^x (ten to centillion)n
higherUnits   1 = "ten"
higherUnits   2 = "hundred"
higherUnits   3 = "thousand"
higherUnits   6 = "million"
higherUnits   9 = "billion"
higherUnits  12 = "trillion"
higherUnits  15 = "quadrillion"
higherUnits  18 = "quintillion"
higherUnits  21 = "sextillion"
higherUnits  24 = "septillion"
higherUnits  27 = "octillion"
higherUnits  30 = "nonillion"
higherUnits  33 = "decillion"
higherUnits  36 = "undecillion"
higherUnits  39 = "duodecillion"
higherUnits  42 = "tredecillion"
higherUnits  45 = "quattuordecillion"
higherUnits  48 = "quindecillion"
higherUnits  51 = "sexdecillion"
higherUnits  54 = "septendecillion"
higherUnits  57 = "octodecillion"
higherUnits  60 = "novemdecillion"
higherUnits  63 = "vigintillion"
higherUnits 100 = "googol"
higherUnits 303 = "centillion"
higherUnits   x = error "Undefined higher unit: 10^" ++ show x

separatorA = " "
separatorB = "-"
negative = "minus"
