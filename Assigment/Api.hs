import Control.Applicative hiding ( many , ( <|> )  )
import Text.Parsec
import Text.Parsec.String (Parser) 
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

--Scheme Code;ISIN Div Payout/ ISIN Growth;ISIN Div Reinvestment;Scheme Name;Net Asset Value;Repurchase Price;Sale Price;Date
--120523;INF846K01ET8;INF846K01EU6;Axis Triple Advantage Fund - Direct Plan - Dividend Option;13.3660;13.2323;13.3660;19-Jun-2015

data MFund = MFund Integer  String String String Double Double Double String deriving (Show) --String String String  deriving (Show)

lexer :: T.TokenParser st
lexer  = T.makeTokenParser emptyDef

natural :: Parser Integer
natural = T.natural lexer

float :: Parser Double
float = T.float lexer

eol :: Parser String
eol = try (string "\r\n")
  <|> try (string "\n")
  <|> try (string "\r")

parseFund :: Parsec String () MFund
parseFund = MFund <$> natural 
                  <*> (char ';' *> (many1 alphaNum <|> string "-")) 
                  <*> (char ';' *> (many1 alphaNum <|> string "-")) 
                  <*> (char ';' *> manyTill anyChar (char ';'))
                  <*> float
                  <*> (char ';' *> float)
                  <*> (char ';' *> float)
                  <*> (char ';' *> many1 (alphaNum <|> char '-'))                  


parseOneBlock :: Parsec String () [MFund]
parseOneBlock = endBy parseFund eol

parseMutualBlock :: Parsec String () [MFund]
parseMutualBlock = 
     space >> eol >>
     manyTill anyChar (char '\n') >>
     space >> eol >>
     parseOneBlock


parseAllBlock :: Parsec String () [MFund]
parseAllBlock = concat <$> many parseMutualBlock

main :: IO ()
main = do 
   input <- readFile "tmp.txt"
   print input
   case parse  parseAllBlock "" input  of
        Left err -> print err
        Right val -> print val
   
