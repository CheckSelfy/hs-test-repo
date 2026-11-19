module Part7.Lang where

import Control.Applicative hiding (optional)
import Data.Char
import Part7.Parser
import Util (notImplementedYet)
import Data.Maybe
import Data.List

-- Parsing JSON files
data JSON
  = JSONObject [(String, JSON)]
  | JSONArray [JSON]
  | JSONString String
  | JSONInteger Integer
  | JSONFloat Double
  | JSONBool Bool
  | JSONNull
  deriving (Show, Eq)

dump :: JSON -> String
dump JSONNull = "null"
dump (JSONBool False) = "false"
dump (JSONBool True) = "true"
dump (JSONFloat float) = show float
dump (JSONInteger int) = show int
dump (JSONString str) = "\"" ++ str ++ "\""
dump (JSONObject xs) = "{" ++ f xs ++ "}"
    where 
        f :: [(String, JSON)] -> String
        f arr = intercalate ", " $ map (\(k, v) -> "\"" ++ k ++ "\": " ++ dump v) arr
dump (JSONArray arr) = "[" ++ f arr  ++ "]"
    where 
        f :: [(JSON)] -> String
        f arr = intercalate ", " $ map dump arr


type JSONParser = String -> Either String JSON

json_parse :: Parser JSON
json_parse = parseJSON <* skipWs <* eofP

parseJSON :: Parser JSON
parseJSON = (
        parseNull <|>
        parseBool <|>
        parseNumber <|>
        parseString <|>
        parseArray <|>
        parseObject)


parseNull :: Parser JSON
parseNull = skipWs *> (stringP "null") *> pure JSONNull

parseBool :: Parser JSON
parseBool = JSONBool <$> (skipWs *> 
    ((stringP "true"  *> pure True) <|> 
     (stringP "false" *> pure False)))

parseNumber :: Parser JSON
parseNumber = skipWs *> (makeNumber <$> signedDigit <*> (optional "" frac) <*> (optional "" exp))
  where
    -- integer
    digits = stringPredicateNonEmptyP isDigit
    signedDigit = ((:) <$> (charP '-') <*> digits) <|> digits

    -- without int part
    frac :: Parser String
    frac = (:) <$> charP '.' <*> digits

    -- exp form
    exp :: Parser (String)
    exp = (\e s d -> e : (maybe d (: d) s))
      <$> (charP 'e' <|> charP 'E')
      <*> optional Nothing (Just <$> (charP '+' <|> charP '-'))
      <*> digits
    
    makeNumber :: String -> String -> String -> JSON
    makeNumber intpart "" "" = (JSONInteger . read) intpart
    makeNumber intpart mfrac mexp = (JSONFloat . read) (intpart ++ mfrac ++ mexp)
  
parseString :: Parser JSON
parseString = skipWs *> (JSONString <$> stringLiteral)

parseArrayOrObject :: Char -> Char -> Char -> ([a] -> JSON) -> (Parser a) -> Parser JSON
parseArrayOrObject lbr sep rbr ctor elem =
  skipWs *> charP lbr *> (ctor <$> sepBy (skipWs *> elem) (skipWs *> charP sep)) <* skipWs <* charP rbr

parseArray :: Parser JSON
parseArray = parseArrayOrObject '[' ',' ']' JSONArray parseJSON

parseObject :: Parser JSON
parseObject = parseArrayOrObject '{' ',' '}' JSONObject parseKV

parseKV :: Parser (String, JSON)
parseKV =
  (,) <$> 
    (skipWs *> stringLiteral)
    <* skipWs <* (charP ':')
    <*> parseJSON

stringLiteral :: Parser String
stringLiteral = charP '"' *> many jsonChar <* charP '"'

jsonChar :: Parser Char
jsonChar = escapedChar <|> normalChar
  where
    normalChar = charPredicateP (\c -> c /= '"' && c /= '\\' && c >= ' ')

    escapedChar = charP '\\' *> (
          charP '"'  *> pure '"'  <|>
          charP '\\' *> pure '\\' <|>
          charP '/'  *> pure '/'  <|>
          charP 'b'  *> pure '\b' <|>
          charP 'f'  *> pure '\f' <|>
          charP 'n'  *> pure '\n' <|>
          charP 'r'  *> pure '\r' <|>
          charP 't'  *> pure '\t'
      )
