module Expression where

import qualified Data.Map as M
import qualified Data.Set as S

-- An Arithmetic Expression is given by the following syntax:
-- e ::= n | x | e1 + e2 | e1 - e2 | e1 * e2
-- For example:
-- The arithmetic expression "x + 1" is represented as:
-- Add (E.Variable "x") (E.Literal 1)
data AExpression
  = Literal Int
  | Variable String
  | Add AExpression AExpression
  | Sub AExpression AExpression
  | Mul AExpression AExpression

-- A Boolean Expression is given by the following syntax:
-- b ::= e1 ≤ e2 | e1 = e2 | b1 ∧ b2
-- For example:
-- The boolean expression "x <= 1" is represented as:
-- Leq (E.Variable "x") (E.Literal 1)
data BExpression
  = Leq AExpression AExpression
  | Equal AExpression AExpression
  | And BExpression BExpression

-- This function returns the set of variables in the AExpression given as input
fvAExp :: AExpression -> S.Set String
fvAExp (Literal n) = S.empty
fvAExp (Variable x) = S.singleton x
fvAExp (Add e1 e2) = S.union (fvAExp e1) (fvAExp e2)
fvAExp (Sub e1 e2) = S.union (fvAExp e1) (fvAExp e2)
fvAExp (Mul e1 e2) = S.union (fvAExp e1) (fvAExp e2)

-- This function returns the set of variables in the BExpression given as input
fvBExp :: BExpression -> S.Set String
fvBExp (Leq e1 e2) = S.union (fvAExp e1) (fvAExp e2)
fvBExp (Equal e1 e2) = S.union (fvAExp e1) (fvAExp e2)
fvBExp (And e1 e2) = S.union (fvBExp e1) (fvBExp e2)

-- A state is a finite mapping from variables to integers
type State = M.Map String Int

-- This function returns the value which an AExpression evaluates to, under
-- a given state
semanticsAExp :: AExpression -> State -> Int
semanticsAExp (Literal n) sigma = n
semanticsAExp (Variable x) sigma = sigma M.! x
semanticsAExp (Add e1 e2) sigma = semanticsAExp e1 sigma + semanticsAExp e2 sigma
semanticsAExp (Sub e1 e2) sigma = semanticsAExp e1 sigma - semanticsAExp e2 sigma
semanticsAExp (Mul e1 e2) sigma = semanticsAExp e1 sigma * semanticsAExp e2 sigma
