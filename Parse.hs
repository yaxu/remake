{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Control.Monad.State (evalState, State, get, put)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Ratio

import NewPattern hiding ((*>),(<*))
import Types

-- ************************************************************ --
-- Parser
--

type Parser = ParsecT Void String (State Type)

-- type Parser = Parsec Void String

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


pClosed :: Parser Code
pClosed = do t <- get
             case t of
               T_Int -> Tk_Int <$> signedInteger
               T_Rational -> Tk_Rational <$> pRatio
               T_String -> Tk_String <$> stringLiteral
               T_Bool -> Tk_Bool True <$ symbol "True"
                         <|> Tk_Bool False <$ symbol "False"
               _ -> do parens $ pFn <|> pClosed

pFn :: Parser Code
pFn =
  do need <- get
     ident <- dbg "identifier" pIdentifier
     (tok, t) <- case (lookup ident functions) of
                   Just (tok, identSig) ->
                     case (fulfill need identSig) of
                       Just t -> return (tok, t)
                       Nothing -> fail $ "Bad type of " ++ ident ++ "\nfound: " ++ show identSig ++ "\ntarget: " ++ show need
                   Nothing -> fail "Unknown function"
     put t
     args (arity t - arity need) tok

args :: Int -> Code -> Parser Code
args 0 tok = return tok
args n tok | n < 0 = error "Internal error, negative args?"
           | otherwise = do (T_F arg result) <- get
                            put arg
                            argtok <- pClosed
                            put result
                            args (n - 1) (Tk_App tok argtok)

pNumber :: Parser ()
pNumber = do optional $ char '-'
             some digit
             optional $ do oneOf ['.', '%']
                           some digit
             return ()
  where digit = oneOf ['0'..'9']


pSlurpTerm :: Parser ()
pSlurpTerm = do parens pSlurpAll
                  <|> brackets pSlurpAll
                  <|> braces pSlurpAll
                  <|> angles pSlurpAll
                  <|> (stringLiteral >> return ())
                  <|> (pIdentifier >> return ())
                  <|> pNumber
                return ()

pSlurpAll :: Parser ()
pSlurpAll = do many (pSlurpTerm <|> (pOp >> return ()))
               return ()

pOp :: Parser String
pOp = symbol "*" <|> symbol "|*"

pFindOp = do many pSlurpTerm
             pOp
