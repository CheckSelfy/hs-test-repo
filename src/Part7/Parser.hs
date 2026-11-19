{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Part7.Parser where
import Control.Applicative (Alternative(..))
import Util (notImplementedYet)
import Data.Char

{-
Парсер представляет из себя функцию из входной строки в один из двух вариантов
1. Остаток строки после разбора и сам результат разбора
2. Строковую ошибку
-}
newtype Parser a = Parser {runParser :: String -> Either String (String, a)}

instance Functor Parser where
  fmap :: forall a b . (a -> b) -> Parser a -> Parser b
  fmap f (Parser r1) = Parser r2
    where 
      r2 :: String -> Either String (String, b)
      r2 s = fmap (\(s, x) -> (s, f x)) (r1 s)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s -> Right (s, x))
  (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser xy) (Parser x) = Parser y
    where 
      y :: String -> Either String (String, b)
      y s = case (xy s) of
        Left e -> Left e
        Right (s1, v1) -> case (x s1) of
          Left e -> Left e
          Right (s2, v2) -> Right (s2, v1 v2)

instance Alternative Parser where
  (<|>) :: forall a. Parser a -> Parser a -> Parser a
  (<|>) (Parser x) (Parser y) = Parser z
    where 
      z :: String -> Either String (String, a)
      z s = case (x s) of
        Left _ -> y s
        a@(Right _) -> a
  empty :: Parser a
  empty = Parser (\s -> Left "no found sorry bruh")

instance Monad Parser where
  (>>=) :: forall a b. Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser x) f = Parser y
    where 
      y :: String -> Either String (String, b)
      y s = case x s of 
        Left e -> Left e
        Right (s1, a) -> let Parser ax = (f a) in ax s1

instance MonadFail Parser where
  fail :: String -> Parser a
  fail err = Parser (\_ -> Left err)

-- | Проверяет что входная строка закончилась, иначе - ошибка
eofP :: Parser ()
eofP = Parser (\x -> if (x == []) then Right ([], ()) else Left "EOF expected")

-- | Пытается прочитать заданный символ, если не получилось - ошибка
charP :: Char -> Parser Char
charP c = Parser f
  where 
    f :: String -> Either String (String, Char)
    f [] = Left $ "Expected " ++ [c] ++ ", found EOF"
    f (x:xs)
      | x == c    = Right (xs, x)
      | otherwise = Left $ "Expected " ++ [c] ++ ", found " ++ [x]


-- | Пытается прочитать любой символ, если не получилось - ошибка
anyCharP :: Parser Char
anyCharP = Parser f
  where 
    f :: String -> Either String (String, Char)
    f [] = Left $ "Expected any char, found EOF"
    f (x:xs) = Right (xs, x)

-- | Пытается прочитать заданную строку, если не получилось - ошибка
stringP :: String -> Parser String
stringP [] = pure []
stringP (x:xs) = (:) <$> charP x <*> stringP xs

-- | Парсит 0 и больше раз `p` разделенный `sepP`.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sepP = sepBy1 p sepP <|> pure []

-- | Парсит 1 и больше раз `p` разделенный `sepP`.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sepP = ((:) <$> p) <*> (many (sepP *> p))

-- | Парсит p, если не получилось возвращает дефолтное значение
optional :: a -> Parser a -> Parser a
optional def p = p <|> pure def

-- Helpers
charPredicateP :: (Char -> Bool) -> Parser Char
charPredicateP pr = Parser f
  where 
    f :: String -> Either String (String, Char)
    f [] = Left $ "Expected any char, found EOF"
    f s@(x:xs) 
      | pr x      = Right (xs, x)
      | otherwise = Left "Not found"

skipWs :: Parser ()
skipWs = many (charPredicateP isSpace) *> pure ()

stringPredicateP :: (Char -> Bool) -> Parser [Char]
stringPredicateP pr = many (charPredicateP pr)

stringPredicateNonEmptyP :: (Char -> Bool) -> Parser [Char]
stringPredicateNonEmptyP pr = some (charPredicateP pr)
