{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

-- (c) Alex McLean 2021
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal2.Pattern where

import Data.Ratio
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Control.Applicative (liftA2)

import Prelude hiding ((<*), (*>))

-- ************************************************************ --
-- Core definition of a Pattern

-- | Timespan (called an arc in tidal v1)
data Span = Span {begin :: Rational, end :: Rational}
  deriving (Show)

-- | An event - a value, its 'whole' timespan, and the timespan that
-- its active (called a 'part' in tidal v1)
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

-- ************************************************************ --

-- | A control pattern as a map from strings to values
type ControlPattern = Pattern (Map.Map String Value)

-- | The possible types of values that can be in a control pattern
data Value = S String
           | F Double
           | R Rational
       deriving (Show)

-- ************************************************************ --

instance Applicative Pattern where
  pure = steady
  (<*>) = app (liftA2 sect)

-- | Apply a pattern of values to a pattern of functions, given a
-- function to merge the 'whole' timespans
app :: (Maybe Span -> Maybe Span -> Maybe Span) -> Pattern (a -> b) -> Pattern a -> Pattern b
app wf patf patv = Pattern f
    where f s = concatMap (\ef -> catMaybes $ map (apply ef) evs) efs
            where efs = (query patf s)
                  evs = (query patv s)
                  apply ef ev = apply' ef ev (maybeSect (active ef) (active ev))
                  apply' ef ev Nothing = Nothing
                  apply' ef ev (Just s') = Just $ Event (wf (whole ef) (whole ev)) s' (value ef $ value ev)

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) = app const

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of values (unrelated to the *> in Prelude)
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) = app (flip const)

-- ************************************************************ --

instance Monad Pattern where
  (>>=) = bind

bind :: Pattern a -> (a -> Pattern b) -> Pattern b
bind = bindWhole (liftA2 sect)

bindInner :: Pattern a -> (a -> Pattern b) -> Pattern b
bindInner = bindWhole const

bindOuter :: Pattern a -> (a -> Pattern b) -> Pattern b
bindOuter = bindWhole (flip const)

bindWhole :: (Maybe Span -> Maybe Span -> Maybe Span) -> Pattern a -> (a -> Pattern b) -> Pattern b
bindWhole chooseWhole pv f = Pattern $ \s -> concatMap (match s) $ query pv s
  where match s e = map (withWhole e) $ query (f $ value e) (active e)
        withWhole e e' = e' {whole = chooseWhole (whole e) (whole e')}

-- ************************************************************ --
-- General hacks

instance Show (a -> b) where
  show _ = "<function>"

-- ************************************************************ --
-- Time utilities

-- | Intersection of two timespans
sect :: Span -> Span -> Span
sect (Span b e) (Span b' e') = Span (max b b') (min e e')

-- | Intersection of two timespans, returns Nothing if they don't intersect
maybeSect :: Span -> Span -> Maybe Span
maybeSect a b = check $ sect a b
  where check :: Span -> Maybe Span
        check (Span a b) | b <= a = Nothing
                         | otherwise = Just (Span a b)


-- | The start of the cycle that a given time value is in
sam :: Rational -> Rational
sam s = toRational $ floor s

-- | The start of the next cycle
nextSam :: Rational -> Rational
nextSam s = sam s + 1

-- | Splits a timespan at cycle boundaries
splitSpans :: Span -> [Span]
splitSpans (Span b e) | e <= b = []
                      | sam b == sam e = [Span b e]
                      | otherwise
  = (Span b (nextSam b)):(splitSpans (Span (nextSam b) e))

withEventSpan :: (Span -> Span) -> Pattern a -> Pattern a
withEventSpan spanf pat = Pattern f
  where f s = map (\e -> e {active = spanf $ active e,
                            whole = spanf <$> whole e
                           }) $ query pat s

withEventTime :: (Rational -> Rational) -> Pattern a -> Pattern a
withEventTime timef pat = Pattern f
  where f s = map (\e -> e {active = withSpanTime timef $ active e,
                            whole = withSpanTime timef <$> whole e
                           }) $ query pat s

withSpanTime :: (Rational -> Rational) -> Span -> Span
withSpanTime timef (Span b e) = Span (timef b) (timef e)

withQuery :: (Span -> Span) -> Pattern a -> Pattern a
withQuery spanf pat = Pattern $ \s -> query pat $ spanf s

withQueryTime :: (Rational -> Rational) -> Pattern a -> Pattern a
withQueryTime timef = withQuery (withSpanTime timef)

-- ************************************************************ --
-- Fundamental patterns

silence :: Pattern a
silence = Pattern (\_ -> [])

-- | Repeat discrete value once per cycle
atom :: a -> Pattern a
atom v = Pattern f
  where f s = map (\s' -> Event (Just $ wholeCycle $ begin s') s' v) (splitSpans s)
        wholeCycle :: Rational -> Span
        wholeCycle t = Span (sam t) (nextSam t)

-- | A continuous value
steady :: a -> Pattern a
steady v = signal (const v)

-- ************************************************************ --
-- Signals

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
toBipolar :: Fractional a => Pattern a -> Pattern a
toBipolar pat = fmap (\v -> (v*2)-1) pat

-- | Converts from a range from -1 to 1, to a range from 0 to 1
fromBipolar :: Fractional a => Pattern a -> Pattern a
fromBipolar pat = fmap (\v -> (v+1)/2) pat

-- | Sawtooth signal
saw :: Pattern Rational
saw = signal $ \t -> mod' t 1

saw2 :: Pattern Rational
saw2 = toBipolar saw

sine :: Fractional a => Pattern a
sine = fromBipolar sine2

sine2 :: Fractional a => Pattern a
sine2 = signal $ \t -> realToFrac $ sin ((pi :: Double) * 2 * fromRational t)

-- ************************************************************ --
-- Pattern manipulations

splitQueries :: Pattern a -> Pattern a
splitQueries pat = Pattern $ \s -> (concatMap (query pat) $ splitSpans s)

-- | Concatenate a list of patterns (works a little differently from
-- 'real' tidal, needs some work)
slowcat :: [Pattern a] -> Pattern a
slowcat pats = splitQueries $ Pattern queryCycle
  where queryCycle s = query (pats !! (mod (floor $ begin s) n)) s
        n = length pats

fast :: Rational -> Pattern a -> Pattern a
fast t pat = withEventTime (/t) $ withQueryTime (*t) $ pat

slow :: Rational -> Pattern a -> Pattern a
slow t = fast (1/t)

early :: Rational -> Pattern a -> Pattern a
early t pat = withEventTime (subtract t) $ withQueryTime (+ t) $ pat

(<~) :: Rational -> Pattern a -> Pattern a
(<~) = early

late :: Rational -> Pattern a -> Pattern a
late t = early (0-t)

(~>) :: Rational -> Pattern a -> Pattern a
(~>) = late

fastcat :: [Pattern a] -> Pattern a
fastcat pats = fast (toRational $ length pats) $ slowcat pats

fastappend :: Pattern a -> Pattern a -> Pattern a
fastappend a b = fastcat [a,b]

slowappend :: Pattern a -> Pattern a -> Pattern a
slowappend a b = slowcat [a,b]

stackPats :: [Pattern a] -> Pattern a
stackPats pats = Pattern $ \s -> concatMap (\pat -> query pat s) pats

squash :: Rational -> Pattern a -> Pattern a
squash into pat = splitQueries $ withEventSpan ef $ withQuery qf pat
  where qf (Span s e) = Span (sam s + (min 1 $ (s - sam s) / into)) (sam s + (min 1 $ (e - sam s) / into))
        ef (Span s e) = Span (sam s + (s - sam s) * into) (sam s + (e - sam s) * into)

squashTo :: Rational -> Rational -> Pattern a -> Pattern a
squashTo b e = late b . squash (e-b)

-- ************************************************************ --
-- Higher order transformations

--every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
--every n f pat = splitQueries $ Pattern 

-- ************************************************************ --

(#) :: ControlPattern -> ControlPattern -> ControlPattern
(#) a b = Map.union <$> a <*> b

(|+|) :: Num a => Pattern a -> Pattern a -> Pattern a
(|+|) a b = (+) <$> a <*> b

(|+) :: Num a => Pattern a -> Pattern a -> Pattern a
(|+) a b = ((+) <$> a) <* b

(+|) :: Num a => Pattern a -> Pattern a -> Pattern a
(+|) a b = ((+) <$> a) *> b

-- ************************************************************ --

sound :: Pattern String -> ControlPattern
sound pat = (Map.singleton "sound" . S) <$> pat

note :: Pattern Double -> ControlPattern
note pat = (Map.singleton "note" . F) <$> pat

-- ************************************************************ --
-- Sequences
--

data Rhythm a = Atom a
  | Silence
  | Subsequence {rSteps   :: [Step a]  }
  | StackCycles {rRhythms :: [Rhythm a]}
  | StackSteps {rPerCycle :: Rational,
                rRhythms  :: [Rhythm a]
               }
  | Patterning {rFunction :: Pattern a -> Pattern a,
                rRhythm :: Rhythm a
               }
  deriving Show

data Step a = Step {sDuration :: Rational,
                    sRhythm :: Rhythm a
                   }
  deriving Show

split :: [Rhythm a] -> Rhythm a
split rs = Subsequence $ map (Step 1) rs

stackCycles :: [Rhythm a] -> Rhythm a
stackCycles = StackCycles

stackSteps :: [Rhythm a] -> Rhythm a
stackSteps [] = Silence
stackSteps rs = StackSteps (stepCount $ head rs) rs

-- | Count the total duration of steps in a rhythm
stepCount :: Rhythm a -> Rational
stepCount (Subsequence ss) = sum $ map sDuration ss
stepCount _ = 1

-- | Turn a rhythm into a pattern
rhythm :: Rhythm a -> Pattern a
rhythm Silence = silence
rhythm (Atom v) = atom v
rhythm r@(Subsequence ss) = stack $ snd $ foldr f (steps,[]) ss
  where f (Step d r) (s,xs) = ((s-d), (squashTo ((s-d)/steps) (s/steps) $ rhythm r):xs)
        steps = stepCount r
rhythm (StackCycles rs) = stack $ map rhythm rs
rhythm (StackSteps _ []) = silence
rhythm (StackSteps spc rs) = stack $ map (\r -> fast (spc/stepCount r) $ rhythm r) rs
rhythm (Patterning f r) = f $ rhythm r

-- ************************************************************ --
-- Functions common to both rhythms and patterns
--

class Common a where
  stack :: [a] -> a

instance Common (Pattern a) where
  stack = stackPats

instance Common (Rhythm a) where
  stack = stackCycles

