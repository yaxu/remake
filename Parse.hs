{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Ratio

import NewPattern
import Types

-- ************************************************************ --
-- Parser
--

type Parser = Parsec Void String

data RhythmT a = Sequence

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens,braces,angles,brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

comma :: Parser String
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
                       denom <- do single '%'
                                   L.decimal
                                <|> return 1
                       return $ num % denom

parseRhythm :: Parser a -> Parser (Rhythm a)
parseRhythm p = (symbol "~" >> return Silence)
                <|> Atom <$> p
                <|> brackets (StackCycles <$> (parseSequence p) `sepBy` comma)
                <|> braces (stackSteps <$> (parseSequence p) `sepBy` comma)
                <|> angles (StackSteps 1 <$> (parseSequence p) `sepBy` comma)
                -- <|> parens

parseIdentifier :: Parser String
parseIdentifier = lexeme $ do x <- (letterChar <|> single '_')
                              xs <- many (alphaNumChar <|> single '_' <|> single '\'')
                              return $ x:xs

parseOperator :: Parser String
parseOperator = lexeme $ some (oneOf ("<>?!|-~+*%$'.#" :: [Char]))

parseExpr :: Sig -> Parser Code
parseExpr targetSig = do ident <- parseIdentifier
                         let matchSig = do (tok, identSig) <- lookup ident functions
                                           sig <- canAs targetSig identSig
                                           return (tok, sig)
                         case matchSig of
                           Just (tok, sig) ->
                             do let d = arity (is sig) - arity (is targetSig)
                                if d > 0
                                  then return tok
                                  else case sig of
                                         (Sig p (T_F a b)) -> do c <- parseExpr $ Sig p b
                                                                 return $ Tk_App tok c
                                         _ -> fail "Internal error asdfasdfx"
                           Nothing -> fail "Internal error agrgergaergaergx"

parseToken :: Sig -> Parser Code
parseToken targetSig = parens $ parseExpr targetSig
  <|> do ident <- parseIdentifier
         let match = do (tok, identSig) <- lookup ident functions
                        asSig <- canAs targetSig identSig
                        return (tok, asSig)
         case match of
           Just (tok, sig) -> if arity (is sig) == arity (is targetSig)
                              then return tok
                              else fail "bah"
           Nothing -> fail "humbug"
           
