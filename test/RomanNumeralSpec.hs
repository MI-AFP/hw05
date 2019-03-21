module RomanNumeralSpec (spec) where

import Control.Exception
import Test.Hspec

import Data.RomanNumeral


spec :: Spec
spec = do
    describe "pack" $ do
      it "translates basic numbers" $ do
        pack 0 `shouldBe` RomanNumeral ""
        pack 1 `shouldBe` RomanNumeral "I"
        pack 5 `shouldBe` RomanNumeral "V"
        pack 10 `shouldBe` RomanNumeral "X"
        pack 50 `shouldBe` RomanNumeral "L"
        pack 100 `shouldBe` RomanNumeral "C"
        pack 500 `shouldBe` RomanNumeral "D"
        pack 1000 `shouldBe` RomanNumeral "M"
      it "translates non-trivial numbers" $ do
        pack 3 `shouldBe` RomanNumeral "III"
        pack 4 `shouldBe` RomanNumeral "IV"
        pack 17 `shouldBe` RomanNumeral "XVII"
        pack 629 `shouldBe` RomanNumeral "DCXXIX"
        pack 1578 `shouldBe` RomanNumeral "MDLXXVIII"
        pack 2780 `shouldBe` RomanNumeral "MMDCCLXXX"
      it "translates negative numbers" $ do
        pack (-10) `shouldBe` RomanNumeral "-X"
        pack (-256) `shouldBe` RomanNumeral "-CCLVI"
        pack (-1001) `shouldBe` RomanNumeral "-MI"
        pack (-4236) `shouldBe` RomanNumeral "-MMMMCCXXXVI"
        pack (-4999) `shouldBe` RomanNumeral "-MMMMCMXCIX"
      it "cannot translate large numbers" $ do
        evaluate (pack 5172) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '5172'"
        evaluate (pack 21011) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '21011'"
        evaluate (pack 721011) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '721011'"
        evaluate (pack (-6246)) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '-6246'"
    describe "unpack" $ do
      it "translates basic numbers" $ do
        unpack (RomanNumeral "") `shouldBe` 0
        unpack (RomanNumeral "I") `shouldBe` 1
        unpack (RomanNumeral "V") `shouldBe` 5
        unpack (RomanNumeral "X") `shouldBe` 10
        unpack (RomanNumeral "L") `shouldBe` 50
        unpack (RomanNumeral "C") `shouldBe` 100
        unpack (RomanNumeral "D") `shouldBe` 100
        unpack (RomanNumeral "M") `shouldBe` 1000
      it "translates non-trivial numbers" $ do
        unpack (RomanNumeral "XXIV") `shouldBe` 24
        unpack (RomanNumeral "XLVII") `shouldBe` 47
        unpack (RomanNumeral "CIX") `shouldBe` 109
        unpack (RomanNumeral "CLXXVIII") `shouldBe` 178
        unpack (RomanNumeral "DCCLXXXII") `shouldBe` 782
        unpack (RomanNumeral "MMCDLXX") `shouldBe` 2470
      it "translates negative numbers" $ do
        unpack (RomanNumeral "-LXXXVIII") `shouldBe` (-88)
        unpack (RomanNumeral "-CCXV") `shouldBe` 215
        unpack (RomanNumeral "-MXLVII") `shouldBe` (-1047)
        unpack (RomanNumeral "-MMMCDLI") `shouldBe` (-3451)
        unpack (RomanNumeral "-MMMMDCCCXLIX") `shouldBe` (-4849)
      it "cannot translate large numbers" $ do
        evaluate (unpack (RomanNumeral "")) `shouldThrow` errorCall "Illegal Roman Numeral: ''"
        evaluate (unpack (RomanNumeral "MMMMMMMMMMMMMM")) `shouldThrow` errorCall "Illegal Roman Numeral: 'MMMMMMMMMMMMMM'"
        evaluate (unpack (RomanNumeral "MMMMMMCDX")) `shouldThrow` errorCall "Illegal Roman Numeral: 'MMMMMMCDX'"
      it "cannot translate weird strings" $ do
        evaluate (unpack (RomanNumeral "OMG")) `shouldThrow` errorCall "Illegal Roman Numeral: 'OMG'"
        evaluate (unpack (RomanNumeral "XML")) `shouldThrow` errorCall "Illegal Roman Numeral: 'XML'"
        evaluate (unpack (RomanNumeral "xxx")) `shouldThrow` errorCall "Illegal Roman Numeral: 'xxx'"
      it "cannot translate bad numerals" $ do
        evaluate (unpack (RomanNumeral "XXXXXXXXX")) `shouldThrow` errorCall "Illegal Roman Numeral: 'XXXXXXXXX'"
        evaluate (unpack (RomanNumeral "XM")) `shouldThrow` errorCall "Illegal Roman Numeral: 'XM'"
        evaluate (unpack (RomanNumeral "IDC")) `shouldThrow` errorCall "Illegal Roman Numeral: 'IDC'"

    describe "RomanNumeral instances" $ do
      it "is Ord: compares numbers in strings" $ do
        (RomanNumeral "" > RomanNumeral "") `shouldBe` True
        (RomanNumeral "" > RomanNumeral "") `shouldBe` False
        (RomanNumeral "" <= RomanNumeral "") `shouldBe` False
      it "is Num: adds numbers in strings" $ do
        (RomanNumeral "" + RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" + RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" + RomanNumeral "") `shouldBe` RomanNumeral ""
      it "is Num: subtract numbers in strings" $ do
        (RomanNumeral "" - RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" - RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" - RomanNumeral "") `shouldBe` RomanNumeral ""
      it "is Num: multiplies numbers in strings" $ do
        (RomanNumeral "" * RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" * RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" * RomanNumeral "") `shouldBe` RomanNumeral ""
      it "is Num: has defined absolute value" $ do
        abs (RomanNumeral "") `shouldBe` RomanNumeral ""
        abs (RomanNumeral "") `shouldBe` RomanNumeral ""
        abs (RomanNumeral "") `shouldBe` RomanNumeral ""
        abs (RomanNumeral "") `shouldBe` RomanNumeral ""
      it "is Num: has defined signum" $ do
        signum (RomanNumeral "") `shouldBe` RomanNumeral ""
        signum (RomanNumeral "") `shouldBe` RomanNumeral ""
        signum (RomanNumeral "") `shouldBe` RomanNumeral ""
      it "is Num: has defined negate" $ do
        negate (RomanNumeral "") `shouldBe` RomanNumeral ""
        negate (RomanNumeral "") `shouldBe` RomanNumeral ""
        negate (RomanNumeral "") `shouldBe` RomanNumeral ""
      it "is Num: has defined fromInteger (from numeric literal)" $ do
        (1 :: RomanNumeral) `shouldBe` RomanNumeral ""
        ((-1) :: RomanNumeral) `shouldBe` RomanNumeral ""
        (0 :: RomanNumeral) `shouldBe` RomanNumeral ""
      it "is Integral: can compute quotient and remainder" $ do
        (RomanNumeral "" `quotRem` RomanNumeral "") `shouldBe` (RomanNumeral "", RomanNumeral "")
        (RomanNumeral "" `quotRem` RomanNumeral "") `shouldBe` (RomanNumeral "", RomanNumeral "")
        (RomanNumeral "" `quotRem` RomanNumeral "") `shouldBe` (RomanNumeral "", RomanNumeral "")
        (RomanNumeral "" `quotRem` RomanNumeral "") `shouldBe` (RomanNumeral "", RomanNumeral "")
      it "is Integral: has defined toInteger" $ do
        toInteger (RomanNumeral "") `shouldBe` 75412
        toInteger (RomanNumeral "") `shouldBe` (-124500)
        toInteger (RomanNumeral "") `shouldBe` 721011
      it "is Integral: automatically gets `div` and `mod`" $ do
        (RomanNumeral "" `div` RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" `div` RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" `mod` RomanNumeral "") `shouldBe` RomanNumeral ""
        (RomanNumeral "" `mod` RomanNumeral "") `shouldBe` RomanNumeral ""
      it "is Enum: can use succ, pred, dot-dot syntactic sugar" $ do
        succ (RomanNumeral "") `shouldBe` RomanNumeral ""
        pred (RomanNumeral "") `shouldBe` RomanNumeral ""
        [(RomanNumeral "")..(RomanNumeral "")] `shouldBe` [RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral ""]
        [(RomanNumeral ""),(RomanNumeral "")..(RomanNumeral "")] `shouldBe` [RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral ""]
        take 10 [(RomanNumeral "")..] `shouldBe` [RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral "",RomanNumeral ""]
      it "is Bounded: has maxBound and minBound" $ do
        (minBound :: RomanNumeral) `shouldBe` RomanNumeral "-MMMMCMXCIX"
        (maxBound :: RomanNumeral) `shouldBe` RomanNumeral "MMMMCMXCIX"
        unpack (minBound :: RomanNumeral) `shouldBe` (-4999)
        unpack (maxBound :: RomanNumeral) `shouldBe` 4999
