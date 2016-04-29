module RegexFinal where


{-

Create a tagless version of Regex

Define a function to optimise your regex.

Probably just try one optimisation to start with!

e.g. many (many r) == r


Hint:

You will realise that you can no longer pattern-match.

Instead, the thing to do is to "make the context explicit".

i.e. create an instance for (Ctx -> r), given an instance for r.

-}

