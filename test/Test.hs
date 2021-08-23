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
   describe "parse" $ do
     it "can parse addition $" $
       parse (pClosed (T_Pattern T_Int)) "" "(pure 3)" `shouldParse` (Tk_App Tk_pure (Tk_Int 3))

