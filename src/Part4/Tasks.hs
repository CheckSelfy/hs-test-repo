{-# LANGUAGE InstanceSigs #-}
module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = reversed (reverse lst)
    where
        reversed [] = REmpty
        reversed (y : ys) = (reversed ys) :< y
    

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show :: ReverseList a -> String
    show x = "[" ++ showImpl x ++ "]"
        where 
            showImpl :: Show a => ReverseList a -> String
            showImpl REmpty = ""
            showImpl (REmpty :< x) = show x
            showImpl (xs :< x) = showImpl xs ++ "," ++ show x

instance Eq a => Eq (ReverseList a) where
    (==) :: Eq a => ReverseList a -> ReverseList a -> Bool
    (==) REmpty REmpty = True
    (==) (xs :< x) (ys :< y) = (x == y) && (xs == ys)
    (==) _ _ = False
    (/=) :: Eq a => ReverseList a -> ReverseList a -> Bool
    (/=) = (not .) . (==)

instance Semigroup (ReverseList a) where
  (<>) :: ReverseList a -> ReverseList a -> ReverseList a
  (<>) x REmpty = x
  (<>) x (ys :< y) = (x <> ys) :< y

instance Monoid (ReverseList a) where
  mempty :: ReverseList a
  mempty = REmpty

instance Functor ReverseList where
  fmap :: (a -> b) -> ReverseList a -> ReverseList b
  fmap _ REmpty = REmpty
  fmap f (xs :< x) = (fmap f xs) :< f x

instance Applicative ReverseList where
  pure :: a -> ReverseList a
  pure x = REmpty :< x
  liftA2 :: (a -> b -> c) -> ReverseList a -> ReverseList b -> ReverseList c
  liftA2 _ REmpty _     = REmpty
  liftA2 f (as :< a) bs = (liftA2 f as bs) <> (fmap (f a) bs)

instance Monad ReverseList where
  (>>=) :: ReverseList a -> (a -> ReverseList b) -> ReverseList b
  (>>=) REmpty _ = REmpty
  (>>=) (as :< a) f = (as >>= f) <> (f a)
