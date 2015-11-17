# Functor

## TODO

* **16.9** - QuickCheck Functor instances
* **16.10** - QuickCheck Exercises
* **16.14** - IO Functor
* **16.15** - Waht if we want to do something different?

The general concept here remains the same: we abstract out a common pattern, make certain it ollows some laws, give it an awesome name and wonder how we ever lived without it. Understanding Functor and Applicative is important to a deep understanding of Monad.

Functor is all about a pattern of mapping over structure. Functors are combinators: they take a sentence or phrase as input and produce a sentence or phrase as an output, with some logical operation applied to the whole. 

## What's a Functor?

A functor is a way to apply a function over or around some structure that we don't want to alter. That is, we want to apply the function to the value that is *inside* some structure and leave the structure alone. The typeclass `Functor` generalizes this pattern. 

```
    class Functor f where
--   [1]    [2]  [3] [4]
	    fmap :: (a -> b) -> f a -> f b
--      [5]       [6]       [7]    [8]
```

* **[1]** - `class` is the keyword to begin the definition of a typeclass.
* **[2]** - Functor is the name of the typeclass we are defining.
* **[3]** - Typeclasses in Haskell usually refer to some sort of *type*. The letters themselves, as with type variables in type signatures, do not mean anything special. `f` is a conventional letter to choose when referring to types that have functorial structure. The `f` must be the same `f` throughout the typeclass definiton. 
* **[4]** - The `where` keyword ends the declaration of the typeclass name and associated types. 
* **[5]** - We begin the declaration of an operation named `fmap`. 
* **[6]** - The argument `(a -> b)` is any function in Hasekll.
* **[7]** - The argument `f a` is a `Functor` `f` that takes a type argument `a`. That is, the `f` is a type that has an instance of the `Functor` typeclass.
* **[8]** - The return value is `f b`. It is the *same* `f` from `f a`, while thetype argument `b` *possibily but not necessarily* refers to a different type.

## There's a whole lot of fmap going around

We can see how the type of `fmap` specializes to different types here:

```
fmap :: (a -> b) ->        f a ->          f b
     :: (a -> b) ->       [] a ->         [] b
	 :: (a -> b) ->    Maybe a ->      Maybe b
	 :: (a -> b) -> Either e a ->   Either e b
	 :: (a -> b) ->     (e,) a ->       (e,) b 
	 :: (a -> b) -> Identity a ->   Identity b
	 :: (a -> b) -> Constant e a -> Constant e b
```

## Left's talk about *f*, baby

As we've said, the `f` in the typeclass definition for `Functor` musst be the same `f` throughout the entire definition, and it must refer to type that implements the typeclass. 

The first thing we know is that our `f` here must have the kind `* -> *`, for a couple of reasons:

* Each argument and result in the type signature for a function must be a fully applied type. Each argument must have the kind `*`.
* The type `f` was applied to a single argument in two different places: `f a` and `f b`. Since both of them must each have the kind `*`, `f` per se must be kind `* -> *`.

### Shining star come into view

Every argument to the type constructor of `->` must be of kind `*`. That is, the kind of `->` is `* -> * -> *`. Each argument and result of every function must be a type constant, not a type constructor. Hence

```
class Functor f where
	fmap :: (a -> b) -> f a -> f b
-- has kind:   *     ->  *  ->  *
```

The type signature of `fmap` tells us that the `f` introduced by the class definition for `Functor` must accept a single type argument and thus be of kind `* -> *`.

e.g.

```
class Sumthin where
	s :: a -> a
--      [1]  [1]

class Else where
	e :: b -> f (g a b c)
--      [2]  [3][4]

class Biffy where
	slayer :: e a b -> (a -> c) -> (b -> d) -> e c d
--           [5]       [6]  [7]
```

* **[1]** - The argument and result type are both `a`. There's nothing else, so `a` must be kind `*`.
* **[2]** - This `b` stands alone as the first argument to `(->)`, so it is kind `*`.
* **[3]** - Here `f` is the outermost type constructor for the second argument of `(->)`. It takes a single argument `(g a b c)`. Thus `f` has kind `* -> *`.
* **[4]** - And `g` is applied to three arguments, `a`, `b`, and `c`. Thus it is kind `* -> * -> * -> *`.
* **[5]** - `e` is an arugment to `(->)` so the application of its arguments must be `*`. Hence `e` is kind `* -> * -> *`.
* **[6]** - It's kind `*`.
* **[7]** - Also, it's kind `*`.

And we know that for the following exampels, it's impossible to infer `v`'s kind because it conflits with itself.

```
class Impish v where
	impossibleKind :: v -> v a

class AlsoImp v where
	nope :: v a -> v
```

### Functor is function application

Back to the type of `fmap`: `fmap :: Functor f = (a -> b) -> f a -> f b`. There's also an infix operator for it, which is `<$>`. And we notice that:

```
(<$>) :: Functor f = (a -> b) -> f a -> f b
 ($)  ::             (a -> b) ->   a ->   b
```

functor is a typeclass for function application *over*, *through* , or *past* some structure `f` that we want to ignore and leave untouched. 

## Functor Laws

* **Identity** - `fmap id == id`
* **Composition** - `fmap (f . g) == fmap f . fmap g`

## The Good, the Bad, and the Ugly

### About Identity Law

```
-- A datatype that has two nullary data constructor and an unary one
data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

-- A law-abiding instance
instance Functor WhoCares where
	fmap _ ItDoesnt = ItDoesnt
	fmap _ WhatThisIsCalled = WhatThisIsCalled
	fmap f (Matter a) = Matter (f a)

-- A law-breaking instance
instance Functor Whoca where
	fmap _ ItDoesnt = WhatThisIsCalled
	fmap _ WhatThisIsCalled = ItDoesnt
	fmap f (Matter a) = Matter (f a)
```

### About Composition Law

```
data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- Super not okay
instance Functor CountingBad where
	fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)
--    (a->b)    f        a         f             b

-- Totes cool
instance Functor CountingBad where
	fmap f (Heisenberg n a) = Heisenberg n (f a)
```

## Commonly used functors

```
> let replaceWithP = const 'p'
> fmap replaceWithP (10, 20)
(10, 'p')
> fmap replaceWithP (10, "woo")
(10, 'p')
```

We'll talk about why it skips the first value in the tuple in a bit. It has to do with the kindedness of tuples and the kindedness of the `f` in Functor.

```
> fmap (+1) negate 10
-9
> fmap (+1) negate -10
11
```

This is to say, `fmap f g = f . g`

### The functors are stacked and that's a fact

```
> fmap rwp lms
"ppp"

> (fmap.fmap) rwp lms
[Just 'p',Nothing,Just 'p']

> (fmap.fmap.fmap) rwp lms
[Just "ppp",Nothing,Just "pppppp"]
```

```
lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP :: b -> Char
replaceWithP = const 'p'

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace
```

> See `ReplacementExperiment.hs` for more detail information.

We notice that the `[]` around `Char` is the *f* of `f Char`, or the structure we lifted over. The *f* of `f a` is the outermost `[]` in `[Maybe [Char]]`. 

## Mapping over the structure to transform the unapplied type argument

Several things we've alread known:

* We've seen that
 * *f* must be a higher-kinded type
 * Functor instances must abide by two laws
* We know that goal of fmapping is to leave the outer structure intact while transforming the type arguments inside.
* We notice that when we `fmap` over a *tuple* or a *either*, it only transforms the second argument.

When we have a datatype that has kind higher than `* -> *`, we must first partialy fill it to make it `* -> *`. See the following example:

```
data Two a b = Two a b deriving (Eq, Show)
data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b) 

instance Functor (Or a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)
```

## QuickCheck Functor instances

## Ignoring possibilities

The Functor instances of `Maybe`, `Either` etc. are handy for times you intentd to ignore the left cases, which are typically your error or failures cases. Because `fmap` doesn't touch those cases, you can map your function right to the valus that you intentd to work with and ignore those failure cases.

> See `IgnoreCases.hs`

## A somewhat surprising functor

Let's see a datatype called `Constant`.

```
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
```

One thing we notice is that the type parameter `b` is a phantom type. It has no corresponding witness at the value/term level. We will explore this later.

Dispite `b` being a phatom type, though, Constant is kind `* -> * -> *`, and that is not a valid Functor. So we must partialy fill it:

```
instance Functor (Constant m) where
	fmap _ (Constant v) = Constant v
```

We can see whatever we `fmap` over Constant, it reamins constant. It's a useful and lawful Functor.

## More structure, more functors

```
data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)
```

## IO Functor

The `IO` type is an abstract datatype; there are no data constructors that you're permitted to match on, so the typeclasses `IO` provides are the only way you can work with values of type `IO a`. 

```
getInt :: IO Int
getInt = fmap read getLine
```

## Waht if we want to do something different?

We talked about Functor as a means of lifting functions over structure so that we may transform only the contents, leaving the structrue alone. Waht if we wanted to transform only the structure and leave the type argument to that structure or type constructor alone? With this, we've arrived at *natural transformations*. We can attempt to put together a type to express what we want:

```
nat :: (f -> g) -> f a -> g a
```

Here `f` and `g` are higher-kinded types. But that makes it impossible to leagally use it. We need more compiler features to support this:

```
{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a. f a -> g a
```

So we're doing the opposite of what a Functor does. We're transforming the structrue, preserving the values as they were. 

## Functors in Haskell are unique for a given datatype

In haskell, Functor instances will be unique for a given datatype. It's not true for Monoid, though. 

It's impossible to do so in Haskell:

```
data Tuple a b = Tuple a b deriving (Eq, Show)

instance Functor (Tuple ? b) where
fmap f (Tuple a b) = Tuple (f a) b
```

We can do this in two ways:
* flip the arguments to the type constructor
* make a new datatype using a `Flip` newtype

```
{-# LANGUAGE FlexibleInstance #-}

module FlipFunctor where

data Tuple a b = Tuple a b deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Tuple a) where
	fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b
```
