import Control.Applicative hiding (some)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import Sound.Tidal2.Pattern
import Sound.Tidal2.Types
import Sound.Tidal2.Parse

main :: IO ()
main = hspec $ do
   describe "Integers" $ do
     it "can parse a pure number as a pattern" $
       parse (pClosed (T_Pattern T_Int)) "" "(pure 3)" `shouldParse` (Tk_App Tk_pure (Tk_Int 3))
     it "can parse addition" $
       parse (pClosed (T_Int)) "" "(4 + 3)" `shouldParse` (Tk_Op (Just $ Tk_Int 4) Tk_plus (Just $ Tk_Int 3))
     it "can parse subtraction" $
       parse (pClosed (T_Int)) "" "(4 - 3)" `shouldParse` (Tk_Op (Just $ Tk_Int 4) Tk_subtract (Just $ Tk_Int 3))
     it "can parse multiplication" $
       parse (pClosed (T_Int)) "" "(4 * 3)" `shouldParse` (Tk_Op (Just $ Tk_Int 4) Tk_multiply (Just $ Tk_Int 3))
     it "can parse division" $
       parse (pClosed (T_Int)) "" "(4 / 3)" `shouldParse` (Tk_Op (Just $ Tk_Int 4) Tk_divide (Just $ Tk_Int 3))
     it "can parse arithmetic in parens" $
       parse (pClosed (T_Int)) "" "(((4 / 3) * 2) + (1+0))" `shouldParse` (Tk_Op (Just (Tk_Op (Just (Tk_Op (Just (Tk_Int 4)) Tk_divide (Just (Tk_Int 3)))) Tk_multiply (Just (Tk_Int 2)))) Tk_plus (Just (Tk_Op (Just (Tk_Int 1)) Tk_plus (Just (Tk_Int 0)))))
     
     
