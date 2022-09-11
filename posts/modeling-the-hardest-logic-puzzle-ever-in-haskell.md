---
title: "Modeling 'The Hardest Logic Puzzle Ever' in Haskell"
date: 2015-08-22
published: true
---

I recently stumbled upon ["The Hardest Logic Puzzle Ever" on
Wikipedia](https://en.wikipedia.org/wiki/The_Hardest_Logic_Puzzle_Ever). I'm
not **that** into the logic puzzles, neither I intended to solve it; but I
have thought that it'd be fun to try representing the question and
the possible answers in it.

<!-- more -->

The puzzle is defined as follows:

> Three gods A, B, and C are called, in no particular order, True,
> False, and Random. True always speaks truly, False always speaks
> falsely, but whether Random speaks truly or falsely is a completely
> random matter. Your task is to determine the identities of A, B, and C
> by asking three yes-no questions; each question must be put to exactly
> one god. The gods understand English, but will answer all questions in
> their own language, in which the words for yes and no are da and ja,
> in some order. You do not know which word means which.

It immediately looks like we can write a DSL where you can ask a god a
question, receive their answers as `Da` or `Ja` and compose an
algorithm to determine the identity of the gods. And we will even be
able to check our solutions via QuickCheck!

Just to catch your interest, here is how we'll be able to  write the
solution to the puzzle on our DSL:

```haskell
-- | https://en.wikipedia.org/wiki/The_Hardest_Logic_Puzzle_Ever#The_solution
solution :: THLPE (GodType, GodType, GodType)
solution = do
  r1 <- askTo GodB $
          ifIAsked GodB (godType GodA <&> (== RandomGod)) <&> (== Ja)
  let notRandomGod = case r1 of Ja -> GodC
                                Da -> GodA

  r2 <- askTo notRandomGod $
          ifIAsked notRandomGod (godType notRandomGod <&> (== FalseGod)) <&> (== Ja)
  let notRandomGodType = case r2 of Ja -> FalseGod
                                    Da -> TrueGod

  r3 <- askTo notRandomGod $
          ifIAsked notRandomGod (godType GodB <&> (== RandomGod)) <&> (== Ja)
  let randomGod = case (r3, notRandomGod) of (Ja, _)    -> GodB
                                             (Da, GodA) -> GodC
                                             (Da, GodC) -> GodA

  let f n =  if | randomGod    == n -> RandomGod
                | notRandomGod == n -> notRandomGodType
                | otherwise         -> if notRandomGodType == TrueGod
                                       then FalseGod else TrueGod

  return $ (f GodA, f GodB, f GodC)

  where (<&>) = flip fmap
```

If you look at the Wikipedia link on the comment, you can see that
this code is the word-by-word translation of the solution on the
page.

----------

Here are the imports I'll use through the program:

```haskell
import           Control.Monad.Trans.RWS (RWS, ask, runRWS, state, tell)
import           Data.Bool               (bool)
import           Data.List               (permutations)
import           Data.Monoid             ((<>))
import           System.Random           (StdGen, getStdRandom, mkStdGen,
                                          random)
import           Test.QuickCheck         (Arbitrary, arbitrary, elements)
```

I started by defining the core types:

```haskell
data GodAnswer
  = Da
  | Ja
  deriving (Show, Eq, Enum)

data GodType
  = TrueGod
  | FalseGod
  | RandomGod
  deriving (Show, Eq, Enum)

data GodName
  = GodA
  | GodB
  | GodC
  deriving (Show, Eq, Enum)
```

And we have some starting conditions per game; the gods identities and
the meanings of `Da` and `Ja`:

```haskell
data THLPESetting
  = THLPESetting { _godA      :: GodType
                 , _godB      :: GodType
                 , _godC      :: GodType
                 , _translate :: Bool -> GodAnswer
                 }
```

At first, my plan was simply using a `Reader THLPESetting` to carry the
`THLPESetting` around the algorithm. But I came onto some obstacles:

* The Random's behavior required a `State` to give different answers
  on each question

* I wanted to hide the game state from the users

* Since gods are omniscient, they should know the game state(the
  other gods types) too.

The latter two points required two different Monad's, one for carrying
the game state between questions(which forbids access to game state),
and one for questions we ask to a god(which has access to game state).

So, I defined the following types:

```haskell
type GodM a = RWS THLPESetting () StdGen a

newtype THLPE a = THLPE { unTHLPE :: GodM a }
  deriving (Functor, Applicative, Monad)

newtype GodQuestion a = GodQuestion { unGodQuestion :: GodM a }
  deriving (Functor, Applicative, Monad)
```

Note: The deriving clauses need `{-# LANGUAGE GeneralizedNewtypeDeriving #-}`.

After that, I defined `godType` accessor:

```haskell
godTypeI :: GodName -> GodM GodType
godTypeI GodA = _godA <$> ask
godTypeI GodB = _godB <$> ask
godTypeI GodC = _godC <$> ask

godType :: GodName -> GodQuestion GodType
godType = GodQuestion . godTypeI
```

As you can see, there is no `GodName -> THLPE GodType`, that means
only gods will know the types of the other gods. `godTypeI` is only
defined for internal use(`askTo` function below) and won't get
exported.

And we have two methods for interacting with gods:

```haskell
askTo :: GodName -> GodQuestion Bool -> THLPE GodAnswer
askTo n (GodQuestion q) = THLPE $ do
  tell 1
  t <- godTypeI n
  translate <- _translate <$> ask
  translate <$> case t of
    TrueGod   -> q
    FalseGod  -> not <$> q
    RandomGod -> state random

ifIAsked :: GodName -> GodQuestion Bool -> GodQuestion GodAnswer
ifIAsked n = GodQuestion  . unTHLPE . askTo n
```

We're almost finished. The only thing left is the function to
interpret our DSL and return if our answer is correct:

```haskell
runTHLPE :: StdGen
         -> THLPESetting
         -> THLPE (GodType, GodType, GodType)
         -> (Bool, (StdGen, Int))
runTHLPE gen set (THLPE r) =
  let (ans, s, w) = runRWS r set gen
  in  (ans == (_godA set, _godB set, _godC set), (s, getSum w))

runTHLPE' :: THLPESetting -> THLPE (GodType, GodType, GodType) -> IO Bool
runTHLPE' s g = getStdRandom $ \gen -> runTHLPE gen s g
```

That's everything we need to run the solution!

```haskell
λ> runTHLPE (THLPESetting TrueGod RandomGod FalseGod (bool Da Ja)) solution
True
λ> runTHLPE (THLPESetting FalseGod TrueGod RandomGod (bool Ja Da)) solution
True
λ> runTHLPE (THLPESetting FalseGod TrueGod RandomGod (bool Ja Da)) solution
True
```

Everything looks okay, but lets test it a little more thoroughly with QuickCheck:

```haskell
data THLPESetting
  = THLPESetting { _godA      :: GodType
                 , _godB      :: GodType
                 , _godC      :: GodType
                 , _translate :: Bool -> GodAnswer
                 }

instance Show THLPESetting where
  show (THLPESetting{..})
    = "THLPESetting { " <> show _godA <>
                   ", " <> show _godB <>
                   ", " <> show _godC <>
                   ", (\\case True  -> " <> show (_translate True) <>
                           "; False -> " <> show (_translate False) <>
                   ")}"

instance Arbitrary THLPESetting where
  arbitrary = do
    [a, b, c] <- elements $ permutations [TrueGod, FalseGod, RandomGod]
    tr        <- elements $ [bool Ja Da, bool Da Ja]
    return $ THLPESetting a b c tr

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary
```

Now we can quickCheck it:

```haskell
λ> :m +Test.QuickCheck
λ> quickCheck (\g s -> fst $ runTHLPE g s solution)
+++ OK, passed 100 tests.
```

That's all!

If you want to tinker with it, the source is at:
<https://gist.github.com/utdemir/1268418421a2ed9ea5f0a57ab0e88551>
