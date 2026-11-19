module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (elemIndex, find, groupBy, partition, sort, maximumBy, group, length)
import Data.Char (digitToInt)
import Data.Ord ( Ord(compare, (<)) )
import Data.Function ( ($), (.), on )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.List.NonEmpty (NonEmpty ((:|)), toList, head)
import Prelude hiding (head)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq numbers = fromMaybe 0 (listToMaybe $ maximumBy (compare `on` length) (groupBy (==) (sort (concatMap (toDigits . abs) numbers))))
    where
        toDigits :: Int -> [Int]
        toDigits 0 = [0]
        toDigits x = toDigitsImpl x

        toDigitsImpl :: Int -> [Int]
        toDigitsImpl 0 = []
        toDigitsImpl x 
            | x < 10 = [x]
            | otherwise = (x `mod` 10) : (toDigits $ x `div` 10)


-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = uniqImpl []
    where
        uniqImpl :: (Eq a) => [a] -> [a] -> [a]
        uniqImpl x [] = x
        uniqImpl x (y:ys)
            | y `within` x = uniqImpl x ys
            | otherwise    = uniqImpl (y : x) ys
        
        within :: (Eq a) => a -> [a] -> Bool
        elem `within` [] = False
        elem `within` (x:xs)
            | x == elem = True
            | otherwise = elem `within` xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = Prelude.map (\g -> (f $ head g, toList g)) $ equivClasses (\x y -> f x == f y) lst
    where
        equivClasses :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
        equivClasses _ [] = []
        equivClasses eq (x:xs) = (x :| matching) : equivClasses eq rest
            where 
                (matching, rest) = Data.List.partition (eq x) xs
