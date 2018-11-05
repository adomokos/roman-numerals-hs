module RomanNumeralsSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

type Roman = String
type Conversions = [(Int, Roman)]

conversions :: Conversions
conversions =
  [ (4, "IV")
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
    it "converts 1 to I" $
      convertToRoman 1 `shouldBe` "I"
    it "converts 2 to II" $
      convertToRoman 2 `shouldBe` "II"
    it "converts 3 to III" $
      convertToRoman 3 `shouldBe` "III"
    it "converts 4 to IV" $
      convertToRoman 4 `shouldBe` "IV"
