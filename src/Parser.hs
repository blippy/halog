module Parser where

import Data.List
import Data.Maybe
--import Text.Parsec.Token as T
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Char as Ch
import Text.ParserCombinators.Parsec.Combinator as Co
import Text.ParserCombinators.Parsec.Prim as Pr
import Text.ParserCombinators.Parsec.Token as T

import Types
--data Date = Date {dtYear::Int, dtMonth::Int, dtDay::Int } deriving (Show)



logLines :: Parser [Request]
logLines = many logLine

logLine :: Parser Request -- NB type specs are mandatory
logLine = do
  --c <- Ch.anyChar
  --1 <- P.count 20 Ch.anyChar -- ignore the 1st 20 chars
  many1 $ noneOf "["
  --Ch.anyChar -- [
  char '['
  -- date <- P.count 11 Ch.anyChar
  (y, m, d) <- dater
  P.count 22 Ch.anyChar -- ignore next bunch of chars
  url <- many $ noneOf " "
  -- P.count 54 $ P.letter --C.anyChar
  --x <- many1 C.anyChar
  --return x

  -- drop rest of line
  many1 $ noneOf "\n"
  optional newline
  
  return $ Request y m d url

monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"
              , "Sep", "Oct", "Nov", "Dec"]
findMonth:: String -> Int -- starting from 1             
findMonth mmm = 1 + (fromJust $ findIndex (== mmm) monthNames)            

dater :: Parser (Int, Int, Int)
dater = do
  d <- P.count 2 Ch.digit
  let d' = read d ::Int
  char '/'

  --m <- Co.choice $ map Ch.string mths -- [Ch.string "Oct"]
  m <- monther
  char '/'
  y <-P.count 4 Ch.digit
  let y' = read y ::Int
  return (y', m, d')



monther :: Parser Int
monther = do
  m <- try (Ch.string "Jan")
       <|> try (Ch.string "Feb")
       <|> try (Ch.string "Mar")
       <|> try (Ch.string "Apr")
       <|> try (Ch.string "May")
       <|> try (Ch.string "Jun")
       <|> try (Ch.string "Jul")
       <|> try (Ch.string "Aug")
       <|> try (Ch.string "Sep")
       <|> try (Ch.string "Oct")
       <|> try (Ch.string "Nov")       
       <|> (Ch.string "Dec")
  return $ findMonth m

m1 = parse monther "m1" "Maz"

d1 = parse dater "" "12/Oct/2015"
--             1         2         3         4
--    1234567890123456789012345678901234567890123456789
t1 = "217.69.133.218 - - [16/Oct/2015:17:51:09 +0100] \"GET /graphics.htm HTTP/1.1\" 200 1003 \"-\" \"Mozilla/5.0 (compatible; Linux x86_64; Mail.RU_Bot/2.0; +http://go.mail.ru/help/robots)\""

t1a = parse logLine "(unknown)" t1


t2 = "178.154.243.97 - - [03/May/2015:06:26:19 +0100] \"GET /money/money.html HTTP/1.1\" 404 151 \"-\" \"Mozilla/5.0 (compatible; YandexBot/3.0; +http://yandex.com/bots)\""

t2a = parse logLine "t2" t2

t3 = parse (Co.choice [Ch.string "Jan", Ch.string "Mar", Ch.string "May"]) "t3" "May"


t4 = parse (try (Ch.string "Jan") <|> try (Ch.string "Mar") <|> try (Ch.string "May")) "t4" "May"

--t5 = parse (do T.reservedNames ["Jan", "Mar", "May"] ) "t5" "May"

t6 = parse monther "t5" "May"

--t5 m =  try (Ch.string) "Jan"

--t5 = parse ((Co.choice $ map (try . Ch.string) ["Jan", "Mar", "May"])) "May"

{-
--parseStr :: String -> [Atom]
parseStr input =
  --let ast= parse commands "(unknown)" $ stripChars "," input in
  let ast = parse logLine "(unknown)" input in
  case ast of
    Left l -> error $ "Failed parsing:" ++  show l
    Right r -> r
      

mainParser = do
  print $ parseStr t1

-}

parseStr :: String -> String -> [Request]
parseStr desc text =
  let ast = parse logLines desc text in
  case ast of
    Left l -> error $ "Failed parsing:" ++  show l
    Right r -> r
