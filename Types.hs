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
  | T_OneOf [Type]
  | T_WildCard
  | T_Param Int
  | T_List Type
  | T_SimpleList Type
  deriving (Eq, Show)

-- Type signature
data Sig = Sig {params :: [Type],
                is :: Type
               }
           deriving Eq

data Code =
  Tk_Int Int | Tk_Rational Rational | Tk_String String | Tk_Float Float |
  Tk_App Code Code |
  Tk_every | Tk_fast |
  Tk_plus
  deriving (Show)

functions :: [(String, (Code, Sig))]
functions =
   [("+", (Tk_plus, numOp)),
    ("every", (Tk_every, pi_pf_p))
   ]
  where pi_pf_p = Sig [T_WildCard] $ T_F (T_Pattern T_Int)
                  (T_F (T_F (T_Pattern $ T_Param 0) (T_Pattern $ T_Param 0))
                    (T_F (T_Pattern $ T_Param 0) (T_Pattern $ T_Param 0))
                  )
        numOp = Sig [T_OneOf[T_Float,T_Int,T_Rational]]
                $ T_F (T_Pattern $ T_Param 0) $ T_F (T_Pattern $ T_Param 0) (T_Pattern $ T_Param 0)
{-
        floatOp = Sig [] $ T_F (T_Pattern T_Float) (T_F (T_Pattern T_Float) (T_Pattern T_Float))
        floatPat = Sig [] $ T_Pattern T_Float
        mapper = Sig [T_WildCard, T_WildCard] $ T_F (T_F (T_Param 0) (T_Param 1)) $ T_F (T_Pattern (T_Param 0)) (T_Pattern (T_Param 1))
        stringToPatMap = Sig [] $ T_F (T_Pattern T_String) (T_Pattern T_Map)
        floatToPatMap = Sig [] $ T_F (T_Pattern T_Float) (T_Pattern T_Map)
        number = OneOf [Pattern Float, Pattern Int]
        number = T_Pattern (T_OneOf[T_Float,T_Int])
-}

arity :: Type -> Int
arity (T_F _ b) = (arity b) + 1
arity _ = 0

resolveParam :: [Type] -> Type -> Type
resolveParam ps (T_Param n) = ps !! n
resolveParam ps (T_F a b) = T_F (resolveParam ps a) (resolveParam ps b)
resolveParam ps (T_OneOf ts) = T_OneOf $ map (resolveParam ps) ts
resolveParam ps (T_Pattern t) = T_Pattern $ resolveParam ps t
resolveParam ps (T_List t) = T_List $ resolveParam ps t
resolveParam _ t = t

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

fits :: Sig -> Sig -> Bool
fits (Sig _ T_WildCard) _ = True
fits _ (Sig _ T_WildCard) = True

fits (Sig pA (T_F a a')) (Sig pB (T_F b b')) =
 (fits (Sig pA a) (Sig pB b)) && (fits (Sig pA a') (Sig pB b'))

fits (Sig pA (T_OneOf as)) (Sig pB (T_OneOf bs)) =
 intersectBy (\a b -> fits (Sig pA a) (Sig pB b)) as bs /= []

fits (Sig pA (T_OneOf as)) (Sig pB b) =
 or $ map (\x -> fits (Sig pA x) (Sig pB b)) as

fits (Sig pA a) (Sig pB (T_OneOf bs)) =
 or $ map (\x -> fits (Sig pA a) (Sig pB x)) bs

fits (Sig pA (T_Pattern a)) (Sig pB (T_Pattern b)) = fits (Sig pA a) (Sig pB b)

fits (Sig pA (T_List a)) (Sig pB (T_List b)) = fits (Sig pA a) (Sig pB b)

fits (Sig pA a) (Sig pB (T_Param b)) = fits (Sig pA a) (Sig pB (pB !! b))
fits (Sig pA (T_Param a)) (Sig pB b) = fits (Sig pA (pA !! a)) (Sig pB b)

-- TODO - could just do
-- fits (Sig _ a) (Sig _ b)   = a == b ?
fits (Sig _ T_Float) (Sig _ T_Float)   = True
fits (Sig _ T_Int) (Sig _ T_Int)       = True
fits (Sig _ T_String) (Sig _ T_String) = True
fits (Sig _ T_Map) (Sig _ T_Map) = True

fits _ _ = False

-- Will either return the target, or a function that (ultimately)
-- returns the target, or nothing
canAs :: Sig -> Sig -> Maybe Sig

-- can function b produce target function a (and how)?
canAs (Sig pA (T_F a a')) (Sig pB (T_F b b')) =
  do -- fit argument
     (Sig _ arg) <- (canAs (Sig pA a) (Sig pB b))
     -- fit result
     (Sig _ result) <- canAs (Sig pA a') (Sig pB b')
     return $ Sig pA (T_F arg result)

-- can function produce target value (and how)?
canAs target@(Sig pA a) (Sig pB (T_F b b')) =
  do (Sig pX x) <- canAs target (Sig pB b')
     return $ Sig pX $ T_F b x

-- A wildcard can produce anything
canAs target (Sig _ T_WildCard) = Just target

-- Check target is a subset
canAs target@(Sig pA (T_OneOf as)) (Sig pB (T_OneOf bs))
  | isSubset = Just target
  | otherwise = Nothing
  where isSubset = length (intersectBy (\a b -> fits (Sig pA a) (Sig pB b)) as bs) == length as

-- Check target is in the 'OneOf'
canAs target (Sig pB (T_OneOf bs)) | isIn = Just target
                                 | otherwise = Nothing
  where isIn = or $ map (\x -> fits target (Sig pB x)) bs

--
canAs (Sig pA (T_Pattern a)) (Sig pB (T_Pattern b)) =
  do (Sig ps t) <- canAs (Sig pA a) (Sig pB b)
     return $ Sig ps (T_Pattern t)

canAs (Sig pA (T_List a)) (Sig pB (T_List b)) =
  do (Sig ps t) <- canAs (Sig pA a) (Sig pB b)
     return $ Sig ps (T_List t)

{-
-- Check target matches the parameter
canAs target@(Sig pA a) from@(Sig pB (Param b))
  = canAs target (Sig (setAt pB b $ resolveParam pA a))
-}

canAs target@(Sig pA a) from@(Sig pB (T_Param b))
  -- If they match, resolve the parameter to the target
  | matches = Just $ Sig (setAt pB b $ resolveParam pA a) (T_Param b)
  | otherwise = Nothing
  where matches = fits target from

canAs target@(Sig pA (T_Param a)) from@(Sig pB b)
  -- If they match, resolve the parameter to the 'from'
  | matches = Just $ Sig (setAt pA a $ resolveParam pB b) (T_Param a)
  | otherwise = Nothing
  where matches = fits target from

canAs target@(Sig _ a) (Sig _ b) | a == b = Just target

canAs _ _ = Nothing
