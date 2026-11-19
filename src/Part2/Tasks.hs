module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm {op = Plus, lhv = a, rhv = b}

(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm {op = Minus, lhv = a, rhv = b}

(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm {op = Times, lhv = a, rhv = b}

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement x@(IntConstant _) = x
replaceVar varName replacement x@(Variable y)
   | varName == y = replacement
   | otherwise = x
replaceVar varName replacement x@(BinaryTerm op lhv rhv) = BinaryTerm op lhv' rhv'
   where
      lhv' = replaceVar varName replacement lhv
      rhv' = replaceVar varName replacement rhv

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate x@(IntConstant _) = x
evaluate x@(Variable _) = x
evaluate x@(BinaryTerm op lhv rhv) = let 
         lhv' = evaluate lhv
         rhv' = evaluate rhv
         op' = case op of
            Plus -> (+)
            Minus -> (-)
            Times -> (*)
      in case (lhv', rhv') of
         (IntConstant a, IntConstant b) -> IntConstant (op' a b)
         _ -> BinaryTerm op lhv' rhv'