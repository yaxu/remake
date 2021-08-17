module Types where

import Data.List (intersectBy, nub, (\\), intercalate)
import NewPattern

-- ************************************************************ --
-- Types of types

data Type =
  T_F Type Type
  | T_String
  | T_Float
  | T_Int
  | T_Rational
  | T_Bool
  | T_Map
  | T_Pattern Type
  | T_Constraint Int
  | T_List Type
  | T_SimpleList Type
  deriving Eq

data Constraint =
  C_OneOf [Type]
  | C_WildCard
  deriving Eq
  
instance Show Type where
 show (T_F a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
 show T_String = "s"
 show T_Float = "f"
 show T_Int = "i"
 show T_Rational = "r"
 show T_Bool = "#"
 show T_Map = "map"
 show (T_Pattern t) = "p [" ++ (show t) ++ "]"
 show (T_Constraint n) = "constraint#" ++ (show n)
 show (T_List t) = "list [" ++ (show t) ++ "]"
 show (T_SimpleList t) = "simplelist [" ++ (show t) ++ "]"

instance Show Constraint where
 show (C_OneOf ts) = "?" ++ show ts
 show C_WildCard = "*"

-- Type signature
data Sig = Sig {constraints :: [Constraint],
                is :: Type
               }
           deriving Eq

instance Show Sig where
   show s = ps ++ (show $ is s)
     where ps | constraints s == [] = ""
              | otherwise = show (constraints s) ++ " => "

data Code =
  Tk_Int Int | Tk_Rational Rational | Tk_String String | Tk_Float Float | Tk_Bool Bool |
  Tk_App Code Code |
  Tk_every | Tk_fast |
  Tk_plus |
  Tk_rev
  deriving (Show)

functions :: [(String, (Code, Sig))]
functions =
   [("+", (Tk_plus, numOp)),
    ("every", (Tk_every, i_pf_p)),
    ("rev", (Tk_rev, pOp))
   ]
  where pi_pf_p = Sig [C_WildCard] $ T_F (T_Pattern T_Int)
                  (T_F (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                    (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                  )
        i_pf_p = Sig [C_WildCard] $ T_F T_Int
                 (T_F (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                   (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                 )
        numOp = Sig [C_OneOf[T_Float,T_Int,T_Rational]]
                $ T_F (T_Pattern $ T_Constraint 0) $ T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0)
        sOp = Sig [] $ T_F (T_Pattern $ T_String) (T_Pattern $ T_String)
        pOp = Sig [C_WildCard] $ T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0)
{-
        floatOp = Sig [] $ T_F (T_Pattern T_Float) (T_F (T_Pattern T_Float) (T_Pattern T_Float))
        floatPat = Sig [] $ T_Pattern T_Float
        mapper = Sig [T_WildCard, T_WildCard] $ T_F (T_F (T_Constraint 0) (T_Constraint 1)) $ T_F (T_Pattern (T_Constraint 0)) (T_Pattern (T_Constraint 1))
        stringToPatMap = Sig [] $ T_F (T_Pattern T_String) (T_Pattern T_Map)
        floatToPatMap = Sig [] $ T_F (T_Pattern T_Float) (T_Pattern T_Map)
        number = OneOf [Pattern Float, Pattern Int]
        number = T_Pattern (T_OneOf[T_Float,T_Int])
-}

arity :: Type -> Int
arity (T_F _ b) = (arity b) + 1
arity _ = 0

isFn :: Type -> Bool
isFn (T_F _ _) = True
isFn _ = False

{-
resolveConstraint :: [Type] -> Type -> Type
resolveConstraint ps (T_Constraint n) = ps !! n
resolveConstraint ps (T_F a b) = T_F (resolveConstraint ps a) (resolveConstraint ps b)
resolveConstraint ps (T_OneOf ts) = T_OneOf $ map (resolveConstraint ps) ts
resolveConstraint ps (T_Pattern t) = T_Pattern $ resolveConstraint ps t
resolveConstraint ps (T_List t) = T_List $ resolveConstraint ps t
resolveConstraint _ t = t
-}

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

fitsConstraint :: Type -> [Constraint]-> Int -> Bool
fitsConstraint t cs i | i >= length cs = error "Internal error - no such constraint"
                      | c == C_WildCard = True
                      | otherwise = or $ map (\t' -> fits t $ Sig cs t') $ options c
   where c = cs !! i
         options (C_OneOf cs) = cs
         options _ = [] -- can't happen..

fits :: Type -> Sig -> Bool
fits t (Sig cs (T_Constraint i)) = fitsConstraint t cs i
fits (T_F arg result) (Sig c (T_F arg' result')) = fits arg (Sig c arg') && fits result (Sig c result')
fits (T_Pattern a) (Sig c (T_Pattern b)) = fits a (Sig c b)
fits (T_List a) (Sig c (T_List b)) = fits a (Sig c b)
fits a (Sig _ b) = a == b

-- How can b produce target a?
-- Will either return the target need, or a function that can
-- return it, or nothing.
fulfill :: Type -> Sig -> Maybe Type
fulfill need@(T_F _ _) contender@(Sig c (T_F arg result))
  | arityD == 0 && fits need contender = Just need
  | arityD > 0 = T_F arg <$> fulfill need (Sig c result)
  | otherwise = Nothing
  where arityD = arity (is contender) - arity need

fulfill need contender | fits need contender = Just need
                       | otherwise = Nothing

