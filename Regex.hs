module Regex where

import ALaCarte hiding (inject)
import Expressions

import           Data.Set    (Set)
import qualified Data.Set as S

-- | Grammar for extended regular expressions
-- Based on Regular expression derivatives, re-examined
--
data Regex = Error           -- matches nothing
           | Accept          -- matches the empty string
           | Char Char       -- matches only "c"
           | Optional Regex  -- matches "" or whatever r matches
           | Many Regex      -- Kleene star
           | Regex :|: Regex  -- union of r and s
           | Regex :&: Regex  -- intersection of r and s
           | Not Regex       -- not r
           | Regex :>: Regex -- r followed by s
  deriving (Show, Read, Eq, Ord)

-- | Partial derivative of r wrt c
-- if we think of r as the set of strings that it matches then
-- derivC r c
-- = | tail r if r begins with c
--   | nothing otherwise
derivC :: Regex -> Char -> Regex
derivC Error _     = Error
derivC Accept _ = Error
derivC (Char c) x
  | c == x    = Accept
  | otherwise = Error
derivC (Optional r) x = derivC r x
derivC (r :|: s) x = (derivC r x) :|: (derivC s x)
derivC (r :&: s) x = (derivC r x) :&: (derivC s x)
derivC (Not r) x = Not (derivC r x)
derivC (r :>: s) x
  | isAccept r =
        ( (derivC r x) :>: s)
        :|:
        (derivC s x)
  | otherwise =
        (derivC r x) :>: s
derivC (Many r) x = (derivC r x) :>: (Many r)

isAccept :: Regex -> Bool
isAccept Error        = False
isAccept Accept    = True
isAccept (Char _)     = False
isAccept (Optional _) = True
isAccept (Many _)     = True
isAccept (r :|: s)     = isAccept r || isAccept s
isAccept (r :&: s)    = isAccept r && isAccept s
isAccept (Not r)      = not (isAccept r)
isAccept (r :>: s)    = isAccept r && isAccept s

isError :: Regex -> Bool
isError Error        = True
isError Accept    = False
isError (Char _)     = False
isError (Optional _) = False
isError (Many _)     = False
isError (r :|: s)     = isError r && isError s
isError (r :&: s)    = isError r || isError s
isError (Not _)      = False -- wrong, but otherwise need to match all strings
isError (r :>: s)    = isError r || isError s

deriv :: Regex -> String -> Regex
deriv = foldl derivC

matches :: Regex -> String -> Bool
matches re str = isAccept $ deriv re str

fromString :: String -> Maybe Regex
fromString = undefined

-- a?bc*
test :: Regex
test = (Optional (Char 'a')) :>: (Char 'b') :>: (Many (Char 'c'))

-- We can already use our regexes to match strings,
-- but want to compile these to finite state machines
-- so they run fast.

data DFA s t = DFA {start :: s, states :: Set s, transition :: s -> t -> s, isFinal :: s -> Bool}

compile :: Regex -> DFA Regex Char
compile = undefined

-- We want to optimise the regex before we compile to a DFA

{-
OPTIMISATION

r & r = r
r & s = s & r
r & (s & t) = (r & s) & t
0 & r = 0
!z & r = r

r + r = r
r + s = s + r
r + (s + t) = (r + s) + t
!0 + r = !0
0 + r = r


(r >> s) >> t = r >> (s >> t)
0 >> r = 0
r >> 0 = 0
e >> r = r
r >> e = r

many (many r) = many r
many e = e
many 0 = e

not (not r) = r

-}

-- some of our regex operations are superfluous
-- e.g. Optional r == r :| Accept

-- Exercise: split Regex up into sub-languages
-- provide an operation which transforms an expression written
-- in the Regex grammar, into one wriiten in a smaller core language

-- implement at least one optimisation operation
-- hint: use pattern matching
opt :: Regex -> Regex
opt = undefined



