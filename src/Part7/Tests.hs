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
run_parse :: String -> Parser a -> Either String (String, a)
run_parse s (Parser f) = f s

unit_eof = do
        isLeft (run_parse "42" eofP) @?= True
        run_parse "" eofP @?= Right("", ())

unit_charP = do
        run_parse "1234" (charP '1') @?= Right ("234", '1')
        isLeft (run_parse "1234" (charP '2')) @?= True
        isLeft (run_parse "1234" (charP '3')) @?= True
        isLeft (run_parse "1234" (charP '4')) @?= True
        isLeft (run_parse "1234" (charP 'x')) @?= True


unit_any_char_p = do
        run_parse "1234" anyCharP @?= Right ("234", '1')
        isLeft (run_parse "" anyCharP) @?= True
    
prop_any_char_p :: Char -> String -> Property
prop_any_char_p symbol string = 
    run_parse (symbol : string) anyCharP === Right(string, symbol)

unit_string_p = do
    run_parse "123456789" (stringP "1234") @?= Right ("56789", "1234")
    run_parse "123456789" (stringP "") @?= Right ("123456789", "")
    isLeft (run_parse "123456789" (stringP "12346789")) @?= True
    isLeft (run_parse "123456789" (stringP "12345678910")) @?= True

prop_string_p :: String -> String -> Property
prop_string_p prefix suffix =
    run_parse (prefix ++ suffix) (stringP prefix) === Right (suffix, prefix)

unit_sep_by :: IO ()
unit_sep_by = do
    run_parse "12, 12, 56" (sepBy (stringP "aboba") (stringP ", ")) @?= Right ("12, 12, 56", [])
    run_parse "12, 12, 56" (sepBy (stringP "12") (stringP ", ")) @?= Right (", 56", ["12", "12"])
    run_parse "12, 12, 56" (sepBy (stringP "12, ") (stringP "")) @?= Right ("56", ["12, ", "12, "])
    run_parse "12, 12, 56" (sepBy (stringPredicateP isDigit) (stringP ", ")) @?= Right ("", ["12", "12", "56"])

unit_sep_by1 :: IO ()
unit_sep_by1 = do
    isLeft (run_parse "12, 12, 56" (sepBy1 (stringP "aboba") (stringP ", "))) @?= True
    run_parse "12, 12, 56" (sepBy1 (stringP "12") (stringP ", ")) @?= Right (", 56", ["12", "12"])
    run_parse "12, 12, 56" (sepBy1 (stringP "12, ") (stringP "")) @?= Right ("56", ["12, ", "12, "])
    run_parse "12, 12, 56" (sepBy1 (stringPredicateP isDigit) (stringP ", ")) @?= Right ("", ["12", "12", "56"])

prop_optional prefix suffix =
    run_parse (prefix ++ suffix) (stringP prefix) === Right (suffix, prefix)

-- Classes tests

unit_functor :: IO ()
unit_functor =
    do
        run_parse string constantParser        @?= Right (string, c)
        run_parse string fmappedConstantParser @?= Right (string, f c)
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
    run_parse input (fmap id p) === run_parse input p
  where
    p = pure c :: Parser Int

prop_functor_compose :: String -> Fun Int Int -> Fun Int Int -> Int -> Property
prop_functor_compose input (Fn f) (Fn g) c =
    run_parse input (fmap (f . g) p) === run_parse input ((fmap f . fmap g) p)
  where
    p = pure c :: Parser Int

-- Applicative

unit_applicative :: IO ()
unit_applicative =
    do
        run_parse string (fParser <*> c1Parser <*> c2Parser) @?= Right (string, f c1 c2)
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
    run_parse input (pure id <*> pure x) === run_parse input (pure x :: Parser Int)

prop_applicative_homomorphism :: String -> Fun Int Int -> Int -> Property
prop_applicative_homomorphism input (Fn f) x =
    run_parse input (pure f <*> pure x) === run_parse input (pure (f x) :: Parser Int)

prop_applicative_binary :: String -> Fun (Int, Int) Int -> Int -> Int -> Property
prop_applicative_binary input (Fn2 f) c1 c2 =
    run_parse input (pure f <*> pure c1 <*> pure c2) === Right (input, f c1 c2)

prop_applicative_interchange :: String -> Fun Int Int -> Int -> Property
prop_applicative_interchange input (Fn f) y =
    run_parse input (u <*> pure y) === run_parse input (pure ($ y) <*> u)
  where
    u = pure f :: Parser (Int -> Int)

-- Alternative
-- A monoid on applicative functors.

unit_alternative :: IO ()
unit_alternative =
    do
        isLeft (run_parse string empty) @?= True
        run_parse string sParser @?= Right (string, 42)
        run_parse string (sParser) @?= run_parse string (sParser <|> empty)
        run_parse string (sParser) @?= run_parse string (empty <|> sParser)
    where
        string = "Garbage"
        (sParser) = (pure 42)

prop_alternative_identity :: String -> Int -> Property
prop_alternative_identity input c = 
        isLeft (run_parse input empty) === True .&&.
        run_parse input (p <|> empty) === run_parse input p .&&.
        run_parse input (empty <|> p) === run_parse input p
  where
    p = pure c :: Parser Int

prop_alternative_choice :: String -> Int -> Int -> Property
prop_alternative_choice input x y =
    run_parse input (pure x <|> pure y) === Right (input, x)

-- Monad

unit_monad :: IO ()
unit_monad =
    do
        run_parse string (return c >>= f) @?= run_parse string (f c)
        run_parse string (m >>= return)   @?= run_parse string m
        run_parse string ((m >>= f) >>= g) @?= run_parse string (m >>= (\x -> f x >>= g))
        run_parse string (m >>= f >>= g)   @?= Right (string, (c + 10) * 2)
        run_parse string (pure "Boom") @?= Right (string, "Boom")
        isLeft (run_parse string (m >> fail "Boom" >>= g))     @?= True
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
    run_parse input (pure a >>= k) === run_parse input (k a)
  where
    k x = pure (f x) 

prop_monad_right_id :: String -> Int -> Property
prop_monad_right_id input a =
    run_parse input (m >>= pure) === run_parse input m
  where
    m = pure a 

prop_monad_assoc :: String -> Int -> Fun Int Int -> Fun Int Int -> Property
prop_monad_assoc input a (Fn f) (Fn g) =
    run_parse input ((m >>= k1) >>= k2) === run_parse input (m >>= (\x -> k1 x >>= k2))
  where
    m    = pure a 
    k1 x = pure (f x) 
    k2 x = pure (g x) 

-- JSON Parser

unit_json_null :: IO ()
unit_json_null = do
    run_parse "null" json_parse @?= Right ("", JSONNull)
    isLeft (run_parse "nul" json_parse) @?= True
    isLeft (run_parse "nulls" json_parse) @?= True

unit_json_bool :: IO ()
unit_json_bool = do
    run_parse "true" json_parse @?= Right ("", JSONBool True)
    run_parse "false" json_parse @?= Right ("", JSONBool False)
    run_parse "  false  " json_parse @?= Right ("", JSONBool False)
    isLeft (run_parse "tru" json_parse) @?= True

unit_json_number :: IO ()
unit_json_number = do
    run_parse "42" json_parse @?= Right ("", JSONInteger 42)
    run_parse "-17" json_parse @?= Right ("", JSONInteger (-17))
    run_parse "3.14" json_parse @?= Right ("", JSONFloat 3.14)
    run_parse "1.5e3" json_parse @?= Right ("", JSONFloat 1500.0)
    run_parse "2E-1" json_parse @?= Right ("", JSONFloat 0.2)
    isLeft (run_parse "3.1.4" json_parse) @?= True

unit_json_string :: IO ()
unit_json_string = do
    run_parse "\"hello\"" json_parse @?= Right ("", JSONString "hello")
    run_parse "\"\"" json_parse @?= Right ("", JSONString "")
    run_parse "\"a\\n\\t\\\\b\"" json_parse @?= Right ("", JSONString "a\n\t\\b")
    isLeft (run_parse "\"unclosed" json_parse) @?= True

unit_json_array :: IO ()
unit_json_array = do
    run_parse "[]" json_parse @?= Right ("", JSONArray [])
    run_parse "[1, 2, 3]" json_parse @?= Right ("", JSONArray [JSONInteger 1, JSONInteger 2, JSONInteger 3])
    run_parse "[true, null, \"x\"]" json_parse @?= Right ("", JSONArray [JSONBool True, JSONNull, JSONString "x"])
    run_parse "[[[]]]" json_parse @?= Right ("", JSONArray [JSONArray [JSONArray []]])
    isLeft (run_parse "[1, 2" json_parse) @?= True

unit_json_object :: IO ()
unit_json_object = do
    run_parse "{}" json_parse @?= Right ("", JSONObject [])
    run_parse "{\"a\": 1}" json_parse @?= Right ("", JSONObject [("a", JSONInteger 1)])
    run_parse "{\"a\": 1, \"b\": false}" json_parse @?= Right ("", JSONObject [("a", JSONInteger 1), ("b", JSONBool False)])
    run_parse "{\"nested\": {\"x\": [null]}}" json_parse @?= Right ("", JSONObject [("nested", JSONObject [("x", JSONArray [JSONNull])])])
    isLeft (run_parse "{\"a\": 1, }" json_parse) @?= True

unit_json_nested :: IO ()
unit_json_nested = do
    run_parse "{\"menu\": {\"items\": [{\"id\": 1}, {\"id\": 2.5}], \"title\": \"hi\"}}"
        json_parse
        @?= Right ("", JSONObject [("menu", JSONObject
            [ 
                ("items", JSONArray [ JSONObject [("id", JSONInteger 1)], JSONObject [("id", JSONFloat 2.5)] ]),
                ("title", JSONString "hi") ])])
    run_parse "  [ 1 , { \"k\" : [ true ] } ]  " json_parse
        @?= Right ("", JSONArray [JSONInteger 1, JSONObject [("k", JSONArray [JSONBool True])]])

unit_json_trailing_garbage :: IO ()
unit_json_trailing_garbage = do
    isLeft (run_parse "1 2" json_parse) @?= True
    isLeft (run_parse "{} []" json_parse) @?= True
    run_parse "  42  " json_parse @?= Right ("", JSONInteger 42)
    run_parse "  42  \n\n\n\t\t\n" json_parse @?= Right ("", JSONInteger 42)

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
    run_parse (dump js) json_parse === Right ("", js)
