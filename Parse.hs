{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Ratio

import NewPattern


-- ************************************************************ --
-- Parser
--

type Parser = Parsec Void Text

data RhythmT a = Sequence


sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens,braces,angles,brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

comma :: Parser Text
comma = symbol ","

parseSequence :: Parser a -> Parser (Rhythm a)
parseSequence p = Subsequence <$> many (parseStep p)

parseStep :: Parser a -> Parser (Step a)
parseStep p = do r <- parseRhythm p
                 d <- do symbol "@"
                         parseRatio
                      <|> return 1
                 return $ Step d r

parseRatio :: Parser Rational
parseRatio = do try $ (toRational <$> L.float)
                <|> do num <- L.decimal
                       denom <- do char '%'
                                   L.decimal
                                <|> return 1
                       return $ num % denom

parseRhythm :: Parser a -> Parser (Rhythm a)
parseRhythm p = (symbol "~" >> return Silence)
                <|> Atom <$> p
                <|> brackets (StackCycles <$> (parseSequence p) `sepBy` comma)
                <|> braces (stackSteps <$> (parseSequence p) `sepBy` comma)
                <|> angles (StackSteps 1 <$> (parseSequence p) `sepBy` comma)
