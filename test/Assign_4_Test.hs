{- Assignment 4 Tests
 - Name: Sarib Kashif
 - Date: November 14 2019
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
--import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

main :: IO ()
main = do print "Performing Value Tests: "
          quickCheck propValue1
          quickCheck propValue2
          quickCheck propValue3
          print "Performing Simp Tests: "
          quickCheck propSimp1
          quickCheck propSimp2
          print "Performing Diff Tests: "
          quickCheck propDiff1
          quickCheck propDiff2
          quickCheck propDiff3
          -- TODO implement real tests

propValue1 :: Double -> Bool
-- check for x^2
propValue1 n = value (Prod X X) n == n ^ 2 

propValue2 :: Double -> Bool
-- Sum X X should be equal to Prod 2 X, so dividing them would be 1
propValue2 n 
    | (n /= 0) = value (Quot (Sum X X) (Prod (Coef 2.0) X)) n == 1.0
    | otherwise = True

propValue3 :: Double -> Bool
-- Ln(e) = 1, and Ln(e^n) = n
propValue3 n = abs (n - value (Log (Exp X)) n) <= 0.0000000000001

propSimp1 :: Double -> Bool
-- check if Sum 0 and constant will be constant
propSimp1 n = abs (exp n - value (simp (Exp (Sum (Prod X (Coef 0.0)) (Coef n)))) n) <= 0.0000000000001

propSimp2 :: Double -> Bool
-- check if Prod of 0 with anything will be 0, Exp of 0 will be 1, Log of 1 will be 0 for any n value
propSimp2 n = value (simp (Log (Exp (Prod X (Coef 0))))) n == 0

propDiff1 :: Double -> Bool
-- check if diff of constant is always 0
propDiff1 n = value (diff (Coef n)) n == 0

propDiff2 :: Double -> Bool
-- check if diff of Log with Exp of constant will be 0 every time
propDiff2 n = value (diff (Sum (Log (Exp (Coef n))) (Coef n))) n == 0.0

propDiff3 :: Double -> Bool
-- check the difference between what it should give (1/n) minus what it actually gives when value is calculated.
-- floating point don't give exact numbers, so I use the difference instead
-- only for non zero numbers
propDiff3 n 
    | (n /= 0) = abs (1/n - value (diff (Quot X (Coef n))) n) <= 0.000000001 
    | otherwise = True