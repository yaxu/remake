{-# LANGUAGE DeriveFunctor #-}

-- (c) Alex McLean 2021
-- Shared under the terms of the GNU Public License v. 3.0

module Pattern where

import Data.Ratio
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import Prelude hiding ((<*), (*>))

-- | Timespan (called an arc in 'real' tidal)
data Span = Span {begin :: Rational, end :: Rational}
  deriving (Show)

-- | An event - a value, its 'whole' timespan, and the timespan that
-- its active (called a 'part' in real tidal)
data Event a = Event {whole :: Maybe Span,
                      active :: Span, value :: a
                     }
  deriving (Show, Functor)

-- | A pattern - a function from a timespan to a list of events active
-- during that timespan
data Pattern a = Pattern {query :: Span -> [Event a]}
  deriving (Functor)

instance Show a => Show (Pattern a) where
  show pat = show $ query pat (Span 0 1)

-- | A control pattern as a map from strings to values
type ControlPattern = Pattern (Map.Map String Value)

-- | The possible types of values that can be in a control pattern
data Value = S String
           | F Double
           | R Rational
       deriving (Show)

-- | A continuous pattern as a function from time to values. Takes the
-- midpoint of the given query as the time value.
signal :: (Rational -> a) -> Pattern a
signal timeF = Pattern {query = f}
  where f (Span b e) = [Event {whole = Nothing,
                               active = (Span b e),
                               value = timeF $ b+((e - b)/2)
                              }
                       ]

-- | Converts from a range from 0 to 1, to a range from -1 to 1
toPolar :: Num a => Pattern a -> Pattern a
toPolar pat = fmap (\v -> (v*2)-1) pat

-- | Sawtooth signal
saw :: Pattern Rational
saw = signal (\t -> mod' t 1)

saw2 :: Pattern Rational
saw2 = toPolar saw

-- | A continuous value
steady :: a -> Pattern a
steady v = signal (const v)

silence :: Pattern a
silence = Pattern (\_ -> [])

-- | The start of the cycle that a given time value is in
sam :: Rational -> Rational
sam s = toRational $ floor s

-- | The start of the next cycle
nextSam :: Rational -> Rational
nextSam s = sam s + 1

-- | Splits a timespan at cycle boundaries
spanCycles :: Span -> [Span]
spanCycles (Span b e) | e <= b = []
                      | sam b == sam e = [Span b e]
                      | otherwise
  = (Span b (nextSam b)):(spanCycles (Span (nextSam b) e))

-- | Repeat discrete value once per cycle
atom :: a -> Pattern a
atom v = Pattern f
  where f s = map (\s' -> Event (Just $ wholeCycle $ begin s') s' v) (spanCycles s)
        wholeCycle :: Rational -> Span
        wholeCycle t = Span (sam t) (nextSam t)

-- | Concatenate a list of patterns (works a little differently from
-- 'real' tidal, needs some work)
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

-- | Intersection of two timespans, returns Nothing if they don't intersect
sect :: Span -> Span -> Maybe Span
sect a b = check $ sect' a b
  where check :: Span -> Maybe Span
        check (Span a b) | b <= a = Nothing
                         | otherwise = Just (Span a b)

-- | Intersection of two timespans
sect' :: Span -> Span -> Span
sect' (Span b e) (Span b' e') = Span (max b b') (min e e')

-- | Apply a pattern of values to a pattern of functions, given a
-- function to merge the 'whole' timespans
app :: (Span -> Span -> Span) -> Pattern (a -> b) -> Pattern a -> Pattern b
app wf patf patv = Pattern f
    where f s = concatMap (\ef -> catMaybes $ map (apply ef) evs) efs
            where efs = (query patf s)
                  evs = (query patv s)
                  apply ef ev = apply' ef ev (sect (active ef) (active ev))
                  apply' ef ev Nothing = Nothing
                  -- TODO - what if one whole is Just, and the other is Nothing ?
                  apply' ef ev (Just s') = Just $ Event (wf <$> (whole ef) <*> (whole ev)) s' (value ef $ value ev)

instance Applicative Pattern where
  pure = steady
  (<*>) = app sect'

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) = app const

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of values (unrelated to the *> in Prelude)
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

