module Part1.Tasks where

import Util(notImplementedYet)
import Data.List (sort)
import Data.Fixed

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = calculateTaylorSeries x' x' (\v n -> -(v * v) / ((2 * n + 2) * (2 * n + 3)))
    where 
        x' = normalizeAngle x

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = calculateTaylorSeries 1.0 x' (\v n -> -(v * v) / ((2 * n + 1) * (2 * n + 2)))
    where 
        x' = normalizeAngle x

normalizeAngle :: Double -> Double
normalizeAngle x = if r > pi then r - twoPi else r
    where 
        twoPi = 2 * pi
        r = x `mod'` twoPi

calculateTaylorSeries :: Double -> Double ->  (Double -> Double -> Double) -> Double
calculateTaylorSeries t0 x ratio = impl t0 0 0.0
    where 
        eps = 1e-12
        impl term n acc
         | abs term < eps = acc + term
         | otherwise = impl nextTerm (n + 1) (acc + term)
         where
            nextTerm = term * ratio x n

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = myGCDImpl (max a' b') (min a' b')
    where 
        a' = abs a
        b' = abs b
        myGCDImpl :: Integer -> Integer -> Integer
        myGCDImpl x 0 = x
        myGCDImpl x y = myGCDImpl y (x `mod` y)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y = isMonthCorrect && isYearCorrect && isDayCorrect
    where
        isMonthCorrect = 1 <= m && m <= 12
        isYearCorrect = 0 < y
        isYearLeap = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)
        daysInMonths = case (m, m `mod` 2) of
            (2, _) -> if isYearLeap then 29 else 28
            (_, 1) -> 31
            otherwise -> 30
        isDayCorrect = 1 <= d && d <= daysInMonths
    

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow b 0 = 1
myPow b n = b * myPow b (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n  
    | n <= 1    = False
    | otherwise = f 2
    where
        f x 
            | x * x > n = True
            | otherwise = (n `mod` x /= 0) && f (x + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = 1/2 * abs (shapeAreaImpl pairs)
    where
        pairs = zip points (drop 1 $ cycle points)
        shapeAreaImpl [] = 0
        shapeAreaImpl (((xi, yi), (xi1, yi1)) : tail) = 
            (xi * yi1 - xi1 * yi) + shapeAreaImpl tail

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind x y z = case (exist, difference) of
        (False, _) -> -1
        (_, 0) -> 2
        (_, d) -> if d > 0 then 0 else 1
    where
        exist = (x + y > z) && (x + z > y) && (y + z > x)
        [a2, b2, c2] = sort [x * x, y * y, z * z]
        difference = c2 - a2 - b2