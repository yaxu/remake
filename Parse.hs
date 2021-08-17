{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Ratio

import NewPattern hiding ((*>),(<*))
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

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed (return ()) $ lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

symbol :: String -> Parser String
symbol = L.symbol sc

parens,braces,angles,brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

pSequence :: Parser a -> Parser (Rhythm a)
pSequence p = Subsequence <$> many (pStep p)

pStep :: Parser a -> Parser (Step a)
pStep p = do r <- pRhythm p
             d <- do void $ symbol "@"
                     pRatio
                  <|> return 1
             return $ Step d r

pRatio :: Parser Rational
pRatio = do try $ (toRational <$> L.float)
            <|> do num <- L.decimal
                   denom <- do single '%'
                               L.decimal
                            <|> return 1
                   return $ num % denom

pRhythm :: Parser a -> Parser (Rhythm a)
pRhythm p = (symbol "~" >> return Silence)
            <|> Atom <$> p
            <|> brackets (StackCycles <$> (pSequence p) `sepBy` comma)
            <|> braces (stackSteps <$> (pSequence p) `sepBy` comma)
            <|> angles (StackSteps 1 <$> (pSequence p) `sepBy` comma)
            -- <|> parens

pIdentifier :: Parser String
pIdentifier = lexeme $ do x <- lowerChar
                          xs <- many $ choice [alphaNumChar,
                                               single '_',
                                               single '\''
                                              ]
                          return $ x:xs

pOperator :: Parser String
pOperator = lexeme $ some (oneOf ("<>?!|-~+*%$'.#" :: [Char]))


pClosed :: Type -> Parser Code
pClosed T_Int = Tk_Int <$> signedInteger
pClosed T_Rational = Tk_Rational <$> pRatio
pClosed T_String = Tk_String <$> stringLiteral
pClosed T_Bool = Tk_Bool True <$ symbol "True"
                 <|> Tk_Bool False <$ symbol "False"
pClosed t = parens $ pFn t <|> pClosed t

pFn :: Type -> Parser Code
pFn need =
  do ident <- dbg "identifier" pIdentifier
     (tok, t) <- case (lookup ident functions) of
                   Just (tok, identSig) ->
                     case (fulfill need identSig) of
                       Just t -> return (tok, t)
                       Nothing -> fail $ "Bad type of " ++ ident ++ "\nfound: " ++ show identSig ++ "\ntarget: " ++ show need
                   Nothing -> fail "Unknown function"
     args t (arity t - arity need) tok

args _ 0 tok = return tok
args t n tok | n < 0 = error "Internal error, negative args?"
             | otherwise = do let (T_F arg result) = t
                              argtok <- pClosed arg
                              args result (n - 1) (Tk_App tok argtok)


