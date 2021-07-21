{-# LANGUAGE DeriveFunctor #-}

-- (c) Alex McLean 2021
-- Shared under the terms of the GNU Public License v. 3.0

module Pattern where

import Data.Ratio
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import Prelude hiding ((<*), (*>))

data Span = Span {begin :: Rational, end :: Rational}
  deriving (Show)

data Event a = Event {whole :: Maybe Span,
                      active :: Span, value :: a
                     }
  deriving (Show, Functor)

data Pattern a = Pattern {query :: Span -> [Event a]}
  deriving (Functor)

instance Show a => Show (Pattern a) where
  show pat = show $ query pat (Span 0 1)

data Value = S String
           | F Double
           | R Rational
       deriving (Show)

type ControlPattern = Pattern (Map.Map String Value)

toPolar :: Num a => Pattern a -> Pattern a
toPolar pat = fmap (\v -> (v*2)-1) pat

signal :: (Rational -> a) -> Pattern a
signal timeF = Pattern {query = f}
  where f (Span b e) = [Event {whole = Nothing,
                               active = (Span b e),
                               value = timeF $ b+((e - b)/2)
                              }
                       ]

saw :: Pattern Rational
saw = signal (\t -> mod' t 1)

saw2 :: Pattern Rational
saw2 = toPolar saw

steady :: a -> Pattern a
steady v = signal (const v)

silence :: Pattern a
silence = Pattern (\_ -> [])

sam :: Rational -> Rational
sam s = toRational $ floor s

nextSam :: Rational -> Rational
nextSam s = sam s + 1

spanCycles :: Span -> [Span]
spanCycles (Span b e) | e <= b = []
                      | sam b == sam e = [Span b e]
                      | otherwise
  = (Span b (nextSam b)):(spanCycles (Span (nextSam b) e))

-- | repeat discrete value once per cycle
atom :: a -> Pattern a
atom v = Pattern f
  where f s = map (\s' -> Event (Just $ wholeCycle $ begin s') s' v) (spanCycles s)
        wholeCycle :: Rational -> Span
        wholeCycle t = Span (sam t) (nextSam t)

slowcat :: [Pattern a] -> Pattern a
slowcat pats = Pattern f
  where f s = concatMap queryCycle $ spanCycles s
        queryCycle s = query (pats !! (mod (floor $ begin s) n)) s
        n = length pats

withEventTime :: (Rational -> Rational) -> Pattern a -> Pattern a
withEventTime timef pat = Pattern f
  where f s = map (\e -> e {active = withSpanTime timef $ active e,
                            whole = withSpanTime timef <$> whole e
                           }) $ query pat s

withQueryTime :: (Rational -> Rational) -> Pattern a -> Pattern a
withQueryTime timef pat = Pattern f
  where f s = query pat (withSpanTime timef s)

withSpanTime :: (Rational -> Rational) -> Span -> Span
withSpanTime timef (Span b e) = Span (timef b) (timef e)

fast :: Rational -> Pattern a -> Pattern a
fast t pat = withEventTime (/t) $ withQueryTime (*t) $ pat

slow :: Rational -> Pattern a -> Pattern a
slow t = fast (1/t)

fastcat :: [Pattern a] -> Pattern a
fastcat pats = fast (toRational $ length pats) $ slowcat pats

fastappend :: Pattern a -> Pattern a -> Pattern a
fastappend a b = fastcat [a,b]

slowappend :: Pattern a -> Pattern a -> Pattern a
slowappend a b = slowcat [a,b]

stack :: [Pattern a] -> Pattern a
stack pats = Pattern $ \s -> concatMap (\pat -> query pat s) pats

sect :: Span -> Span -> Maybe Span
sect a b = check $ sect' a b
  where check :: Span -> Maybe Span
        check (Span a b) | b <= a = Nothing
                         | otherwise = Just (Span a b)

sect' :: Span -> Span -> Span
sect' (Span b e) (Span b' e') = Span (max b b') (min e e')

instance Applicative Pattern where
  pure = steady
  (<*>) = app sect'

app :: (Span -> Span -> Span) -> Pattern (a -> b) -> Pattern a -> Pattern b
app wf patf patv = Pattern f
    where f s = concatMap (\ef -> catMaybes $ map (apply ef) evs) efs
            where efs = (query patf s)
                  evs = (query patv s)
                  apply ef ev = apply' ef ev (sect (active ef) (active ev))
                  apply' ef ev Nothing = Nothing
                  apply' ef ev (Just s') = Just $ Event (wf <$> (whole ef) <*> (whole ev)) s' (value ef $ value ev)

(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) = app const

(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) = app (flip const)

sound :: Pattern String -> ControlPattern
sound pat = (Map.singleton "sound" . S) <$> pat

note :: Pattern Double -> ControlPattern
note pat = (Map.singleton "note" . F) <$> pat

(#) :: ControlPattern -> ControlPattern -> ControlPattern
(#) a b = Map.union <$> a <*> b

(|+|) :: Num a => Pattern a -> Pattern a -> Pattern a
(|+|) a b = (+) <$> a <*> b

(|+) :: Num a => Pattern a -> Pattern a -> Pattern a
(|+) a b = ((+) <$> a) <* b

(+|) :: Num a => Pattern a -> Pattern a -> Pattern a
(+|) a b = ((+) <$> a) *> b

