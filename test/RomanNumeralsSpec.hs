module RomanNumeralsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)

main :: IO ()
main = hspec spec

type Roman = String
type Conversions = [(Int, Roman)]

conversions :: Conversions
conversions =
  [ (90, "XC")
  , (50, "L")
  , (40, "XL")
  , (10, "X")
  , (9, "IX")
  , (5, "V")
  , (4, "IV")
  , (1, "I") ]

convertToRoman :: Int -> Roman
convertToRoman 0 = []
convertToRoman x =
  roman ++ convertToRoman (x - number)
    where
      (number, roman) =
        head . filter (\(a,_) -> a <= x) $ conversions

convertFromRoman :: Roman -> Int
convertFromRoman "" = 0
convertFromRoman r =
  number + convertFromRoman (drop (length roman) r)
    where
      (number, roman) = fromJust $ find (\(_,r') -> r' `isPrefixOf` r) conversions

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

prop_convertNumber :: Int -> Bool
prop_convertNumber x = (convertFromRoman . convertToRoman) x == x


spec :: Spec
spec = do
  describe "Converting to Roman Numerals" $ do
    it "converts 1 to I" $
      convertToRoman 1 `shouldBe` "I"
    it "converts 2 to II" $
      convertToRoman 2 `shouldBe` "II"
    it "converts 3 to III" $
      convertToRoman 3 `shouldBe` "III"
    it "converts 4 to IV" $
      convertToRoman 4 `shouldBe` "IV"
    it "converts 5 to V" $
      convertToRoman 5 `shouldBe` "V"
    it "converts 6 to VI" $
      convertToRoman 6 `shouldBe` "VI"
    it "converts 8 to VIII" $
      convertToRoman 8 `shouldBe` "VIII"
    it "converts 9 to IX" $
      convertToRoman 9 `shouldBe` "IX"
    it "converts 10 to X" $
      convertToRoman 10 `shouldBe` "X"
    it "converts 11 to XI" $
      convertToRoman 11 `shouldBe` "XI"
    it "converts 99 to L" $
      convertToRoman 99 `shouldBe` "XCIX"

  describe "Roman to Number Conversions" $ do
    it "converts I to 1" $
      convertFromRoman "I" `shouldBe` 1
    it "converts II to 2" $
      convertFromRoman "II" `shouldBe` 2
    it "converts III to 3" $
      convertFromRoman "III" `shouldBe` 3
    it "converts IV to 4" $
      convertFromRoman "IV" `shouldBe` 4
    it "converts V to 5" $
      convertFromRoman "V" `shouldBe` 5
    it "converts VIII to 8" $
      convertFromRoman "VIII" `shouldBe` 8
    it "converts IX to 9" $
      convertFromRoman "IX" `shouldBe` 9
    it "converts X to 10" $
      convertFromRoman "X" `shouldBe` 10
    it "converts XI to 11" $
      convertFromRoman "XI" `shouldBe` 11
    it "converts XCIX to 99" $
      convertFromRoman "XCIX" `shouldBe` 99
