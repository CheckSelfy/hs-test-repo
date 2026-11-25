module Part7.Tests where
import Part7.Parser
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Either
import Control.Applicative
import Data.Char
import Part7.Lang (JSON(JSONNull, JSONBool, JSONFloat, JSONInteger, JSONObject, JSONArray, JSONString), json_parse, dump)
import Data.List (intersperse, intercalate)
import qualified Part7.Parser as JSON

-- Parser tests
unit_eof = do
        isLeft (runParser eofP "42") @?= True
        runParser eofP "" @?= Right("", ())

unit_charP = do
        runParser (charP '1') "1234" @?= Right ("234", '1')
        isLeft (runParser (charP '2') "1234") @?= True
        isLeft (runParser (charP '3') "1234") @?= True
        isLeft (runParser (charP '4') "1234") @?= True
        isLeft (runParser (charP 'x') "1234") @?= True


unit_any_char_p = do
        runParser anyCharP "1234" @?= Right ("234", '1')
        isLeft (runParser anyCharP "") @?= True
    
prop_any_char_p :: Char -> String -> Property
prop_any_char_p symbol string = 
    runParser anyCharP (symbol : string) === Right(string, symbol)

unit_string_p = do
    runParser (stringP "1234") "123456789" @?= Right ("56789", "1234")
    runParser (stringP "") "123456789" @?= Right ("123456789", "")
    isLeft (runParser (stringP "12346789") "123456789") @?= True
    isLeft (runParser (stringP "12345678910") "123456789") @?= True

prop_string_p :: String -> String -> Property
prop_string_p prefix suffix =
    runParser (stringP prefix) (prefix ++ suffix) === Right (suffix, prefix)

unit_sep_by :: IO ()
unit_sep_by = do
    runParser (sepBy (stringP "aboba") (stringP ", ")) "12, 12, 56" @?= Right ("12, 12, 56", [])
    runParser (sepBy (stringP "12") (stringP ", ")) "12, 12, 56" @?= Right (", 56", ["12", "12"])
    runParser (sepBy (stringP "12, ") (stringP "")) "12, 12, 56" @?= Right ("56", ["12, ", "12, "])
    runParser (sepBy (stringPredicateP isDigit) (stringP ", ")) "12, 12, 56" @?= Right ("", ["12", "12", "56"])

unit_sep_by1 :: IO ()
unit_sep_by1 = do
    isLeft (runParser (sepBy1 (stringP "aboba") (stringP ", ")) "12, 12, 56") @?= True
    runParser (sepBy1 (stringP "12") (stringP ", ")) "12, 12, 56" @?= Right (", 56", ["12", "12"])
    runParser (sepBy1 (stringP "12, ") (stringP "")) "12, 12, 56" @?= Right ("56", ["12, ", "12, "])
    runParser (sepBy1 (stringPredicateP isDigit) (stringP ", ")) "12, 12, 56" @?= Right ("", ["12", "12", "56"])

prop_optional prefix suffix =
    runParser (stringP prefix) (prefix ++ suffix) === Right (suffix, prefix)

-- Classes tests

unit_functor :: IO ()
unit_functor =
    do
        runParser constantParser string        @?= Right (string, c)
        runParser fmappedConstantParser string @?= Right (string, f c)
    where
        string = "Garbage"
        (f, c)                   = ((* 42), 932)
        (fmappedConstantParser, constantParser) = (fmap f $ pure c, pure c)

-- Furthermore f needs to adhere to the following:
-- Identity
-- fmap id == id
-- Composition
-- fmap (f . g) == fmap f . fmap g

prop_functor_id :: String -> Int -> Property
prop_functor_id input c =
    runParser (fmap id p) input === runParser p input
  where
    p = pure c :: Parser Int

prop_functor_compose :: String -> Fun Int Int -> Fun Int Int -> Int -> Property
prop_functor_compose input (Fn f) (Fn g) c =
    runParser (fmap (f . g) p) input === runParser ((fmap f . fmap g) p) input
  where
    p = pure c :: Parser Int

-- Applicative

unit_applicative :: IO ()
unit_applicative =
    do
        runParser (fParser <*> c1Parser <*> c2Parser) string @?= Right (string, f c1 c2)
    where
        string = "Garbage"
        (f, c1, c2)                   = ((*), 932, 24)
        (fParser, c1Parser, c2Parser) = (pure f, pure c1, pure c2)

-- Further, any definition must satisfy the following:
-- Identity
-- pure id <*> v = v
-- Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- Homomorphism
-- pure f <*> pure x = pure (f x)
-- Interchange
-- u <*> pure y = pure ($ y) <*> u

prop_applicative_identity :: String -> Int -> Property
prop_applicative_identity input x =
    runParser (pure id <*> pure x) input === runParser (pure x :: Parser Int) input

prop_applicative_homomorphism :: String -> Fun Int Int -> Int -> Property
prop_applicative_homomorphism input (Fn f) x =
    runParser (pure f <*> pure x) input === runParser (pure (f x) :: Parser Int) input

prop_applicative_binary :: String -> Fun (Int, Int) Int -> Int -> Int -> Property
prop_applicative_binary input (Fn2 f) c1 c2 =
    runParser (pure f <*> pure c1 <*> pure c2) input === Right (input, f c1 c2)

prop_applicative_interchange :: String -> Fun Int Int -> Int -> Property
prop_applicative_interchange input (Fn f) y =
    runParser (u <*> pure y) input === runParser (pure ($ y) <*> u) input
  where
    u = pure f :: Parser (Int -> Int)

-- Alternative
-- A monoid on applicative functors.

unit_alternative :: IO ()
unit_alternative =
    do
        isLeft (runParser empty string) @?= True
        runParser sParser string @?= Right (string, 42)
        runParser (sParser) string @?= runParser (sParser <|> empty) string
        runParser (sParser) string @?= runParser (empty <|> sParser) string
    where
        string = "Garbage"
        (sParser) = (pure 42)

prop_alternative_identity :: String -> Int -> Property
prop_alternative_identity input c = 
        isLeft (runParser empty input) === True .&&.
        runParser (p <|> empty) input === runParser p input .&&.
        runParser (empty <|> p) input === runParser p input
  where
    p = pure c :: Parser Int

prop_alternative_choice :: String -> Int -> Int -> Property
prop_alternative_choice input x y =
    runParser (pure x <|> pure y) input === Right (input, x)

-- Monad

unit_monad :: IO ()
unit_monad =
    do
        runParser (return c >>= f) string @?= runParser (f c) string
        runParser (m >>= return) string   @?= runParser m string
        runParser ((m >>= f) >>= g) string @?= runParser (m >>= (\x -> f x >>= g)) string
        runParser (m >>= f >>= g) string   @?= Right (string, (c + 10) * 2)
        runParser (pure "Boom") string @?= Right (string, "Boom")
        isLeft (runParser (m >> fail "Boom" >>= g) string)     @?= True
    where
        string = "Garbage"
        c      = 42
        m      = pure c
        f x    = pure (x + 10)
        g x    = pure (x * 2)

-- Instances of Monad should satisfy the following:

-- Left identity
-- return a >>= k = k a
-- Right identity
-- m >>= return = m
-- Associativity
-- m >>= (\x -> k x >>= h) = (m >>= k) >>= h

prop_monad_left_id :: String -> Int -> Fun Int Int -> Property
prop_monad_left_id input a (Fn f) =
    runParser (pure a >>= k) input === runParser (k a) input
  where
    k x = pure (f x) 

prop_monad_right_id :: String -> Int -> Property
prop_monad_right_id input a =
    runParser (m >>= pure) input === runParser m input
  where
    m = pure a 

prop_monad_assoc :: String -> Int -> Fun Int Int -> Fun Int Int -> Property
prop_monad_assoc input a (Fn f) (Fn g) =
    runParser ((m >>= k1) >>= k2) input === runParser (m >>= (\x -> k1 x >>= k2)) input
  where
    m    = pure a 
    k1 x = pure (f x) 
    k2 x = pure (g x) 

-- JSON Parser

unit_json_null :: IO ()
unit_json_null = do
    runParser json_parse "null" @?= Right ("", JSONNull)
    isLeft (runParser json_parse "nul") @?= True
    isLeft (runParser json_parse "nulls") @?= True

unit_json_bool :: IO ()
unit_json_bool = do
    runParser json_parse "true" @?= Right ("", JSONBool True)
    runParser json_parse "false" @?= Right ("", JSONBool False)
    runParser json_parse "  false  " @?= Right ("", JSONBool False)
    isLeft (runParser json_parse "tru") @?= True

unit_json_number :: IO ()
unit_json_number = do
    runParser json_parse "42" @?= Right ("", JSONInteger 42)
    runParser json_parse "-17" @?= Right ("", JSONInteger (-17))
    runParser json_parse "3.14" @?= Right ("", JSONFloat 3.14)
    runParser json_parse "1.5e3" @?= Right ("", JSONFloat 1500.0)
    runParser json_parse "2E-1" @?= Right ("", JSONFloat 0.2)
    isLeft (runParser json_parse "3.1.4") @?= True

unit_json_string :: IO ()
unit_json_string = do
    runParser json_parse "\"hello\"" @?= Right ("", JSONString "hello")
    runParser json_parse "\"\"" @?= Right ("", JSONString "")
    runParser json_parse "\"a\\n\\t\\\\b\"" @?= Right ("", JSONString "a\n\t\\b")
    isLeft (runParser json_parse "\"unclosed") @?= True

unit_json_array :: IO ()
unit_json_array = do
    runParser json_parse "[]" @?= Right ("", JSONArray [])
    runParser json_parse "[1, 2, 3]" @?= Right ("", JSONArray [JSONInteger 1, JSONInteger 2, JSONInteger 3])
    runParser json_parse "[true, null, \"x\"]" @?= Right ("", JSONArray [JSONBool True, JSONNull, JSONString "x"])
    runParser json_parse "[[[]]]" @?= Right ("", JSONArray [JSONArray [JSONArray []]])
    isLeft (runParser json_parse "[1, 2") @?= True

unit_json_object :: IO ()
unit_json_object = do
    runParser json_parse "{}" @?= Right ("", JSONObject [])
    runParser json_parse "{\"a\": 1}" @?= Right ("", JSONObject [("a", JSONInteger 1)])
    runParser json_parse "{\"a\": 1, \"b\": false}" @?= Right ("", JSONObject [("a", JSONInteger 1), ("b", JSONBool False)])
    runParser json_parse "{\"nested\": {\"x\": [null]}}" @?= Right ("", JSONObject [("nested", JSONObject [("x", JSONArray [JSONNull])])])
    isLeft (runParser json_parse "{\"a\": 1, }") @?= True

unit_json_nested :: IO ()
unit_json_nested = do
    runParser json_parse "{\"menu\": {\"items\": [{\"id\": 1}, {\"id\": 2.5}], \"title\": \"hi\"}}"
        @?= Right ("", JSONObject [("menu", JSONObject
            [ 
                ("items", JSONArray [ JSONObject [("id", JSONInteger 1)], JSONObject [("id", JSONFloat 2.5)] ]),
                ("title", JSONString "hi") ])])
    runParser json_parse "  [ 1 , { \"k\" : [ true ] } ]  "
        @?= Right ("", JSONArray [JSONInteger 1, JSONObject [("k", JSONArray [JSONBool True])]])

unit_json_trailing_garbage :: IO ()
unit_json_trailing_garbage = do
    isLeft (runParser json_parse "1 2") @?= True
    isLeft (runParser json_parse "{} []") @?= True
    runParser json_parse "  42  " @?= Right ("", JSONInteger 42)
    runParser json_parse "  42  \n\n\n\t\t\n" @?= Right ("", JSONInteger 42)

-- json property based test

instance Arbitrary JSON where 
    arbitrary = sized genJSON

genJSON :: Int -> Gen JSON
genJSON 0 = oneof [ 
    pure JSONNull, 
    JSONBool <$> arbitrary, 
    JSONInteger <$> arbitrary, 
    JSONFloat <$> arbitrary, 
    JSONString <$> genSafeString 
    ]
genJSON n = oneof [ 
    genJSON 0,
    JSONArray <$> resize (n `div` 2) (listOf subJSON),
    JSONObject <$> resize (n `div` 2) (listOf ((,) <$> genSafeString <*> subJSON)) ]
    where
        subJSON = genJSON (n `div` 2)

genSafeString :: Gen String
genSafeString = listOf (suchThat arbitrary (\c -> c >= ' ' && c /= '"' && c /= '\\'))

prop_code_encode :: JSON -> Property
prop_code_encode js =
    runParser json_parse (dump js) === Right ("", js)
