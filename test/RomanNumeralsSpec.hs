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

numbers :: [Int]
numbers = [1..1000]

genNumbers :: Gen Int
genNumbers = elements numbers

prop_convertNumber :: Property
prop_convertNumber =
  forAll genNumbers
    (\x ->
      (convertFromRoman . convertToRoman) x == x)

spec :: Spec
spec = do
  describe "Converting to Roman Numerals" $ do
    it "converts number to Roman and back" $ property $
      prop_convertNumber
