{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map hiding (foldr)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       width   :: mx -> Int
       height  :: mx -> Int
       getElem :: mx -> Int -> Int -> Int
       fromGenerator :: Int -> Int -> (Int -> Int -> Int) -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  width _ = 1
  height _ = 1
  getElem m 0 0 = m
  fromGenerator 1 1 f = f 0 0

instance Matrix [[Int]] where
       width [] = 0
       width (r:_) = length r
       height = length
       getElem m x y = (m !! y) !! x
       fromGenerator w h f = [[ f x y | x <- [0..w-1]] | y <- [0..h-1]]

instance Matrix (SparseMatrix Int) where
       width = sparseMatrixWidth
       height = sparseMatrixHeight
       getElem m x y = findWithDefault 0 (x, y) (sparseMatrixElements m)
       fromGenerator w h f = SparseMatrix w h $ foldr step empty [(x, y) | x <- [0..w-1], y <- [0..h-1]]
              where 
                     step (x, y) acc = let val = f x y 
                      in if val == 0 then acc else insert (x, y) (f x y) acc

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = fromGenerator w w (\x y -> if (x == y) then 1 else 0)
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = fromGenerator w h (\_ _ -> 0)
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix m1 m2 = 
       fromGenerator (width m2) (height m1) (\x y -> foldr (+) 0 [ getElem m1 k y * getElem m2 x k | k <- [0..(width m1 - 1)] ])
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m = laplace n (\x y -> getElem m x y)
       where
              n = width m
              laplace 0 _ = 1
              laplace 1 f = f 0 0
              laplace k f = sum [ (-1)^j * f j 0 * laplace (k - 1) (minor j) | j <- [0..k-1] ]
                     where minor j x y = f (if x < j then x else x + 1) (y + 1)
