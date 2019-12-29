{- Assignment 4
 - Name: Sarib Kashif
 - Date: November 14 2019
 -}
module Assign_4 where

macid :: String
macid = "kashis2"

data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - value
 - -----------------------------------------------------------------
 - for any constructor that is being used, call the function again 
   seperately for each component of the constructor, and then adding
   the right operator to connect the components.
   e.g Sum X X == X + X         <- not meant to be syntactically correct
 - Coef a where a is a constant returns a
 - X returns the n that the user inputted
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value (Coef a) n = a
value X n = n
value (Sum a b) n = (value a n) + (value b n)
value (Prod a b) n = (value a n) * (value b n)
value (Quot a b) n = (value a n) / (value b n)
value (Exp a) n = exp (value a n)
value (Log a) n = log (value a n)

{- -----------------------------------------------------------------
 - simp
 - -----------------------------------------------------------------
 - apply the 0 and 1 simplification rules such as X + 0 = X
 - if none of the simplification rules can be applied, try to
   apply the rules within the specific constructor to see if any
   other rules can be applied. 
   E.g Log (Sum X Coef 0) can be simplified to Log X
 - if can't be simplified further, you reached the simplest case
 -}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Coef x) = (Coef x)
simp X = X
simp (Sum (Coef 0.0) u) = u
simp (Sum u (Coef 0.0)) = u
simp (Sum u v) = 
    let
        u' = simp u
        v' = simp v
    in
        if u' == u && v' == v
            then (Sum u v)
            else  simp (Sum u' v')
simp (Prod (Coef 0.0) u) = (Coef 0.0)
simp (Prod u (Coef 0.0)) = (Coef 0.0)
simp (Prod (Coef 1.0) u) = u
simp (Prod u (Coef 1.0)) = u
simp (Prod u v) = 
    let
        u' = simp u
        v' = simp v
    in
        if u' == u && v' == v
            then (Prod u v)
            else  simp (Prod u' v')
simp (Quot u (Coef 1.0)) = u
simp (Quot u v) = 
    let
        u' = simp u
        v' = simp v
    in
        if u' == u && v' == v
            then (Quot u v)
            else  simp (Quot u' v')
simp (Exp (Coef 0.0)) = (Coef 1.0)
simp (Exp (u)) =
    let
        u' = simp u
    in
        if u' == u
            then (Exp u)
            else  simp (Exp u')
simp (Log (Coef 1.0)) = (Coef 0.0)
simp (Log (u)) =
    let
        u' = simp u
    in
        if u' == u
            then (Log u)
            else  simp (Log u')

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - apply differentiation rules for each constructor
 - diff constant is 0, diff X is 1, diff of X + Y is X' + Y', etc
 - eventually the function reaches the base cases of X or Constant
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff (Coef u) = Coef 0.0
diff X = Coef 1.0
diff (Sum u v) = simp $ Sum (diff u) (diff v)
diff (Prod u v) = simp $ Sum (Prod (diff u) v) (Prod u (diff v))
diff (Quot u v) = simp $ Quot (Sum (Prod (diff u) v) (Prod (Coef (-1.0)) (Prod u (diff v)))) (Prod v v)
diff (Exp u) = simp $ Prod (Exp (u)) (diff u)
diff (Log u) = simp $ Quot (diff u) (u)

{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - read the file that has the string (f)
 - read it again in the form of MathExpr Double
 - in file g, write the derivative of the string
   which was extracted from file f
 -}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g = do
    u <- readFile f 
    let v = read u :: MathExpr Double
    writeFile g (show (diff v))

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------

    TEST CASES

Function: value
Test Case Number: 1
Input: (Log X) -2
Expected Output: NaN
Actual Output: NaN

Function: value
Test Case Number: 2
Input: (Prod X (Sum (Coef (-2) Exp X))) 5
Expected Output: 732.065795512883
Actual Output: 732.065795512883

Function: value
Test Case Number: 3
Input: (Quot X (Coef (-2))) (-1)
Expected Output: 0.5
Actual Output: 0.5

Function: simp
Test Case Number: 1
Input: (Sum (Prod X (Coef 0)) (Coef (-2)))
Expected Output: Coef (-2.0)
Actual Output: Coef (-2.0)

Function: simp
Test Case Number: 2
Input: (Log (Exp (Coef 0)))
Expected Output: Coef 0.0
Actual Output: Coef 0.0

Function: simp
Test Case Number: 3
Input: (Quot (Sum X X) (Coef (1)))
Expected Output: Sum X X
Actual Output: Sum X X

Function: diff
Test Case Number: 1
Input: (Log (Exp (Sum (Coef 5) (Prod X (Coef (-2))))))
Expected Output: Quot (Prod (Exp (Sum (Coef 5.0) (Prod X (Coef (-2.0))))) (Coef (-2.0))) (Exp (Sum (Coef 5.0) (Prod X (Coef (-2.0)))))
Actual Output: Quot (Prod (Exp (Sum (Coef 5.0) (Prod X (Coef (-2.0))))) (Coef (-2.0))) (Exp (Sum (Coef 5.0) (Prod X (Coef (-2.0)))))

Function: diff
Test Case Number: 2
Input: (Sum (Prod X (Coef (-1))) (Coef 5))
Expected Output: Coef (-1.0)
Actual Output: Coef (-1.0)

Function: diff
Test Case Number: 3
Input: (Log (Prod X (Coef (-2))))
Expected Output: Quot (Coef (-2.0)) (Prod X (Coef (-2.0)))
Actual Output: Quot (Coef (-2.0)) (Prod X (Coef (-2.0)))

    QUICK CHECK TEST CASES

Function: propValue1
Property: value (Prod X X) n == n ^ 2 
Actual Test Result: Pass

Function: propValue2
Property: (n /= 0) = value (Quot (Sum X X) (Prod (Coef 2.0) X)) n == 1.0
Actual Test Result: Pass

Function: propValue3
Property: abs (n - value (Log (Exp X)) n) <= 0.0000000000001
Actual Test Result: Pass

Function: propSimp1
Property: abs (exp n - value (simp (Exp (Sum (Prod X (Coef 0.0)) (Coef n)))) n) <= 0.0000000000001
Actual Test Result: Pass

Function: propSimp2
Property: value (simp (Log (Exp (Prod X (Coef 0))))) n == 0
Actual Test Result: Pass

Function: propDiff1
Property: value (diff (Coef n)) n == 0
Actual Test Result: Pass

Function: propDiff2
Property: value (diff (Sum (Log (Exp (Coef n))) (Coef n))) n == 0.0
Actual Test Result: Pass

Function: propDiff3
Property: (n /= 0) = abs (1/n - value (diff (Quot X (Coef n))) n) <= 0.000000001 
Actual Test Result: Pass

 - -----------------------------------------------------------------
 - TODO: add test cases
 -}

