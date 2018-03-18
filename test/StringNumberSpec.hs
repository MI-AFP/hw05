module StringNumberSpec (spec) where

import Test.Hspec

import Data.StringNumber


spec :: Spec
spec = do
    describe "numToEnglishString" $ do
      it "translates basic numbers" $ do
        numToEnglishString 0 `shouldBe` "zero"
        numToEnglishString 1 `shouldBe` "one"
        numToEnglishString 7 `shouldBe` "seven"
        numToEnglishString 11 `shouldBe` "eleven"
        numToEnglishString 15 `shouldBe` "fifteen"
        numToEnglishString 18 `shouldBe` "eighteen"
      it "translates whole tens" $ do
        numToEnglishString 10 `shouldBe` "ten"
        numToEnglishString 40 `shouldBe` "forty"
        numToEnglishString 60 `shouldBe` "sixty"
        numToEnglishString 90 `shouldBe` "ninety"
      it "translates whole higher whole units" $ do
        numToEnglishString 100 `shouldBe` "one hundred"
        numToEnglishString 1000 `shouldBe` "one thousand"
        numToEnglishString (10^9) `shouldBe` "one billion"
        numToEnglishString (10^51) `shouldBe` "one sexdecillion"
        numToEnglishString (10^100) `shouldBe` "one googol"
        numToEnglishString (10^303) `shouldBe` "one centillion"
      it "translates up to hundred" $ do
        numToEnglishString 25 `shouldBe` "twenty-five"
        numToEnglishString 31 `shouldBe` "thirty-one"
        numToEnglishString 46 `shouldBe` "forty-six"
        numToEnglishString 59 `shouldBe` "fifty-nine"
        numToEnglishString 66 `shouldBe` "sixty-six"
        numToEnglishString 92 `shouldBe` "ninety-two"
      it "translates hundreds" $ do
        numToEnglishString 125 `shouldBe` "one hundred twenty-five"
        numToEnglishString 301 `shouldBe` "three hundred one"
        numToEnglishString 460 `shouldBe` "four hundred sixty"
        numToEnglishString 519 `shouldBe` "five hundred nineteen"
        numToEnglishString 666 `shouldBe` "six hundred sixty-six"
        numToEnglishString 800 `shouldBe` "eight hundred"
      it "translates thousands" $ do
        numToEnglishString 1025 `shouldBe` "one thousand twenty-five"
        numToEnglishString 2301 `shouldBe` "two thousand three hundred one"
        numToEnglishString 4060 `shouldBe` "four thousand sixty"
        numToEnglishString 5172 `shouldBe` "five thousand one hundred seventy-two"
        numToEnglishString 6600 `shouldBe` "six thousand six hundred"
        numToEnglishString 8000 `shouldBe` "eight thousand"
        numToEnglishString 21011 `shouldBe` "twenty-one thousand eleven"
        numToEnglishString 60000 `shouldBe` "sixty thousand"
        numToEnglishString 75412 `shouldBe` "seventy-five thousand four hundred twelve"
        numToEnglishString 124500 `shouldBe` "one hundred twenty four thousand five hundred"
        numToEnglishString 721011 `shouldBe` "seven hundred twenty-one thousand eleven"
      it "translates milions and billions" $ do
        numToEnglishString 1256721 `shouldBe` "one million two hundred fifty-six thousand seven hundred twenty-one"
        numToEnglishString 31286721 `shouldBe` "thirty-one million two hundred eighty-six thousand seven hundred twenty-one"
        numToEnglishString 631256761 `shouldBe` "six hundred thirty-one million two hundred fifty-six thousand seven hundred sixty-one"
        numToEnglishString 1492638526 `shouldBe` "one billion four hundred ninety-two million six hundred thirty-eight thousand five hundred twenty-six"
        numToEnglishString 41402638720 `shouldBe` "forty-one billion four hundred two million six hundred thirty-eight thousand sevej hundred twenty"
      it "translates huge numbers" $ do
        numToEnglishString 5000000045000000111000000000000000002 `shouldBe` "five undecillion fourty-five octillion one hundred eleven quintillion two"
        numToEnglishString (10^100 + 27) `shouldBe` "one googol twenty-seven"
        numToEnglishString (2*10^303 + 256*10^27) `shouldBe` "two centillion two hundred fifty-six octillion"
      it "translates negative numbers" $ do
        numToEnglishString (-7) `shouldBe` "minus seven"
        numToEnglishString (-163) `shouldBe` "minut one hundred sixty-three"
        numToEnglishString (-631256717) `shouldBe` "six hundred thirty-one million two hundred fifty-six thousand seven hundred seventeen"

    describe "englishStringToInt" $ do
      it "translates basic numbers" $ do
        englishStringToInt "zero" `shouldBe` 0
        englishStringToInt "one" `shouldBe` 1
        englishStringToInt "seven" `shouldBe` 7
        englishStringToInt "eleven" `shouldBe` 11
        englishStringToInt "fifteen" `shouldBe` 15
        englishStringToInt "eighteen" `shouldBe` 18
      it "translates whole tens" $ do
        englishStringToInt "ten" `shouldBe` 10
        englishStringToInt "forty" `shouldBe` 40
        englishStringToInt "sixty" `shouldBe` 60
        englishStringToInt "ninety" `shouldBe` 90
      it "translates whole higher whole units" $ do
        englishStringToInt "one hundred" `shouldBe` 100
        englishStringToInt "one thousand" `shouldBe` 1000
        englishStringToInt "one billion" `shouldBe` 10^9
        englishStringToInt "one sexdecillion" `shouldBe` 10^51
        englishStringToInt "one googol" `shouldBe` 10^100
        englishStringToInt "one centillion" `shouldBe` 10^303
      it "translates up to hundred" $ do
        englishStringToInt "twenty-five" `shouldBe` 25
        englishStringToInt "thirty-one" `shouldBe` 31
        englishStringToInt "forty-six" `shouldBe` 46
        englishStringToInt "fifty-nine" `shouldBe` 59
        englishStringToInt "sixty-six" `shouldBe` 66
        englishStringToInt "ninety-two" `shouldBe` 92
      it "translates hundreds" $ do
        englishStringToInt "one hundred twenty-five" `shouldBe` 125
        englishStringToInt "three hundred one" `shouldBe` 301
        englishStringToInt "four hundred sixty" `shouldBe` 460
        englishStringToInt "five hundred nineteen" `shouldBe` 519
        englishStringToInt "six hundred sixty-six" `shouldBe` 666
        englishStringToInt "eight hundred" `shouldBe` 800
      it "translates thousands" $ do
        englishStringToInt "one thousand twenty-five" `shouldBe` 1025
        englishStringToInt "two thousand three hundred one" `shouldBe` 2301
        englishStringToInt "four thousand sixty" `shouldBe` 4060
        englishStringToInt "five thousand one hundred seventy-two" `shouldBe` 5172
        englishStringToInt "six thousand six hundred" `shouldBe` 6600
        englishStringToInt "eight thousand" `shouldBe` 8000
        englishStringToInt "twenty-one thousand eleven" `shouldBe` 21011
        englishStringToInt "sixty thousand" `shouldBe` 60000
        englishStringToInt "seventy-five thousand four hundred twelve" `shouldBe` 75412
        englishStringToInt "one hundred twenty four thousand five hundred" `shouldBe` 124500
        englishStringToInt "seven hundred twenty-one thousand eleven" `shouldBe` 721011
      it "translates milions and billions" $ do
        englishStringToInt "one million two hundred fifty-six thousand seven hundred twenty-one" `shouldBe` 1256721
        englishStringToInt "thirty-one million two hundred eighty-six thousand seven hundred twenty-one" `shouldBe` 31286721
        englishStringToInt "six hundred thirty-one million two hundred fifty-six thousand seven hundred sixty-one" `shouldBe` 631256761
        englishStringToInt "one billion four hundred ninety-two million six hundred thirty-eight thousand five hundred twenty-six" `shouldBe` 1492638526
        englishStringToInt "forty-one billion four hundred two million six hundred thirty-eight thousand sevej hundred twenty" `shouldBe` 41402638720
      it "translates huge numbers" $ do
        englishStringToInt "five undecillion fourty-five octillion one hundred eleven quintillion two" `shouldBe` 5000000045000000111000000000000000002
        englishStringToInt "one googol twenty-seven" `shouldBe` (10^100 + 27)
        englishStringToInt "two centillion two hundred fifty-six octillion" `shouldBe` (2*10^303 + 256*10^27)
      it "translates negative numbers" $ do
        englishStringToInt "minus seven" `shouldBe` (-7)
        englishStringToInt "minut one hundred sixty-three" `shouldBe` (-163)
        englishStringToInt "six hundred thirty-one million two hundred fifty-six thousand seven hundred seventeen" `shouldBe` (-631256717)

      describe "StringNumber instances" $ do
        it "is Ord: compares numbers in strings" $ do
          (StringNumber "sixty-six" > StringNumber "eleven") `shouldBe` True
          (StringNumber "minus sixty-six" > StringNumber "eleven") `shouldBe` False
          (StringNumber "googol" <= StringNumber "three hundred forty-seven") `shouldBe` False
        it "is Num: adds numbers in strings" $ do
          (StringNumber "sixty-six" + StringNumber "eleven") `shouldBe` StringNumber "seventy-seven"
          (StringNumber "minus sixty-six" + StringNumber "five") `shouldBe` StringNumber "minus sixty-one"
          (StringNumber "twenty-five million" + StringNumber "googol") `shouldBe` StringNumber "one googol twenty-five million"
        it "is Num: subtract numbers in strings" $ do
          (StringNumber "sixty-six" - StringNumber "eleven") `shouldBe` StringNumber "fifty-five"
          (StringNumber "minus sixty-six" - StringNumber "five") `shouldBe` StringNumber "minus seventy-one"
          (StringNumber "twenty-five million" - StringNumber "three million seven hundred thousand") `shouldBe` StringNumber "twenty-one million three hundred thousand"
        it "is Num: multiplies numbers in strings" $ do
          (StringNumber "five" * StringNumber "eleven") `shouldBe` StringNumber "fifty-five"
          (StringNumber "minus six" * StringNumber "five") `shouldBe` StringNumber "minus thirty"
          (StringNumber "two hundred seven" * StringNumber "ten") `shouldBe` StringNumber "two thousand seventy"
        it "is Num: has defined absolute value" $ do
          abs (StringNumber "two hundred forty-one") `shouldBe` StringNumber "two hundred forty-one"
          abs (StringNumber "minus ten") `shouldBe` StringNumber "ten"
          abs (StringNumber "zero") `shouldBe` StringNumber "zero"
          abs (StringNumber "minus twenty-nine") `shouldBe` StringNumber "twenty-nine"
        it "is Num: has defined signum" $ do
          signum (StringNumber "five thousand four hundred twelve") `shouldBe` StringNumber "one"
          signum (StringNumber "minus seventeen") `shouldBe` StringNumber "minus one"
          signum (StringNumber "zero") `shouldBe` StringNumber "zero"
        it "is Num: has defined negate" $ do
          negate (StringNumber "five thousand four hundred twelve") `shouldBe` StringNumber "minus five thousand four hundred twelve"
          negate (StringNumber "minus seventeen") `shouldBe` StringNumber "seventeen"
          negate (StringNumber "zero") `shouldBe` StringNumber "zero"
        it "is Num: has defined fromInteger (from numeric literal)" $ do
          (75412 :: StringNumber) `shouldBe` StringNumber "seventy-five thousand four hundred twelve"
          ((-124500) :: StringNumber) `shouldBe` StringNumber "minus one hundred twenty four thousand five hundred"
          (721011 :: StringNumber) `shouldBe` StringNumber "seven hundred twenty-one thousand eleven"
        it "is Integral: can compute quotient and remainder" $ do
          (StringNumber "eleven" `quotRem` StringNumber "five") `shouldBe` (StringNumber "two", StringNumber "one")
          (StringNumber "seven" `quotRem` StringNumber "minus two") `shouldBe` (StringNumber "minus three", StringNumber "one")
          (StringNumber "minus fifteen" `quotRem` StringNumber "six") `shouldBe` (StringNumber "minus two", StringNumber "minus three")
          (StringNumber "minus ten" `quotRem` StringNumber "minut three") `shouldBe` (StringNumber "three", StringNumber "minus one")
        it "is Integral: has defined toInteger" $ do
          toInteger (StringNumber "seventy-five thousand four hundred twelve") `shouldBe` 75412
          toInteger (StringNumber "minus one hundred twenty four thousand five hundred") `shouldBe` (-124500)
          toInteger (StringNumber "seven hundred twenty-one thousand eleven") `shouldBe` 721011
        -- TODO: test Enum and Real
