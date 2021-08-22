module Parse where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Control.Monad.State (evalState, State, get, put)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Ratio
import Control.Monad (unless)

import NewPattern hiding ((*>),(<*))
import Types

-- ************************************************************ --
-- Parser
--

-- type Parser = ParsecT Void String (State Type)

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
pClosed need = do dbg "pClosed" $ case need of
                    T_Int -> Tk_Int <$> signedInteger
                    T_Rational -> Tk_Rational <$> pRatio
                    T_String -> Tk_String <$> stringLiteral
                    T_Bool -> Tk_Bool True <$ symbol "True"
                      <|> Tk_Bool False <$ symbol "False"
                    _ -> do parens $ pFn need <|> pInfix need <|> pClosed need

pInfix :: Type -> Parser Code
pInfix need = do (tok, T_F a (T_F b c)) <- pPeekOp need
                 a' <- dbg "left" $ optional $ try $ pClosed a -- 'try' needed due to numerical + prefix..
                 pOp
                 b' <- optional $ try $ pClosed b
                 let t = case (a', b') of
                           (Nothing, Nothing) -> (T_F a $ T_F b c)
                           (Just _, Nothing) -> (T_F b c)
                           (Nothing, Just _) -> (T_F a c)
                           (Just _, Just _) -> c
                 unless (need == t) $ fail "TODO helpful error message around operator arity"
                 return $ Tk_Op a' tok b'

pFn :: Type -> Parser Code
pFn need =
  do ident <- dbg "identifier" pIdentifier
     (tok, t) <- case (lookup ident functions) of
                   Just (tok, Prefix, identSig) ->
                     case (fulfill need identSig) of
                       Just t -> return (tok, t)
                       Nothing -> fail $ "Bad type of " ++ ident ++ "\nfound: " ++ show identSig ++ "\ntarget: " ++ show need
                   Nothing -> fail "Unknown function"
                   _ -> fail "TODO - handle infix section"
     args t (arity t - arity need) tok

args :: Type -> Int -> Code -> Parser Code
args _ 0 tok = return tok
args need n tok | n < 0 = error "Internal error, negative args?"
                | otherwise = do let (T_F arg result) = need
                                 argtok <- pClosed arg
                                 args result (n - 1) (Tk_App tok argtok)

pNumber :: Parser ()
pNumber = lexeme $ do optional $ char '-'
                      some digit
                      optional $ try $ do oneOf ['.', '%']
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
                  <|> try pNumber
                return ()

pSlurpAll :: Parser ()
pSlurpAll = do many (pSlurpTerm <|> (pOp >> return ()))
               return ()

pOp :: Parser String
pOp = lexeme $ some $ oneOf "!#$%&*+./<=>?@\\^|-~"

pPeekOp :: Type -> Parser (Code, Type)
pPeekOp need = dbg "pPeekOp" $ try $ lookAhead $
  do many pSlurpTerm
     op <- pOp
     case (lookup op functions) of
       Just (tok, Infix, opSig) ->
         case (fulfill need opSig) of
           Just t -> return (tok, t)
           Nothing -> fail $ "Bad type of op " ++ op ++ "\nfound: " ++ show opSig ++ "\ntarget: " ++ show need
       Nothing -> fail $ "Unknown operator " ++ op
       _ -> fail "Unexpected prefix function.."
             
