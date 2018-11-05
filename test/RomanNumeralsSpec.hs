module RomanNumeralsSpec where

import Test.Hspec

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
convertFromRoman "" = undefined

spec :: Spec
spec =
  describe "Converting to Roman Numerals" $ do
    it "converts 1 to I" $ do
      convertToRoman 1 `shouldBe` "I"
    it "converts 2 to II" $ do
      convertToRoman 2 `shouldBe` "II"
    it "converts 3 to III" $ do
      convertToRoman 3 `shouldBe` "III"
    it "converts 4 to IV" $ do
      convertToRoman 4 `shouldBe` "IV"
    it "converts 5 to V" $ do
      convertToRoman 5 `shouldBe` "V"
    it "converts 6 to VI" $ do
      convertToRoman 6 `shouldBe` "VI"
    it "converts 8 to VIII" $ do
      convertToRoman 8 `shouldBe` "VIII"
    it "converts 9 to IX" $ do
      convertToRoman 9 `shouldBe` "IX"
    it "converts 10 to X" $ do
      convertToRoman 10 `shouldBe` "X"
    it "converts 11 to XI" $ do
      convertToRoman 11 `shouldBe` "XI"
    it "converts 99 to L" $ do
      convertToRoman 99 `shouldBe` "XCIX"
