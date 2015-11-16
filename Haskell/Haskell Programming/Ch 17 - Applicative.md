# Applicative

Applicative is a *monoidal functor*. The `Applicative` typeclass allows for function application lifted over structrue (like Functor). But with Applicative the function we're applying is also embeded in some structure. Because both the function and the value it's being applied to have structure, we have to smash those strucrures together. So, Applicative involves monoids and functors.


## Defining Applicative

```
class Functor f => Applicative f where
	pure  :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```

The first thing you're going to notice about this typeclass declaration is that the `f` that represents the structure, and itself constrained by the `Functor` typeclass. So every type that can have an `Applicative` instance must also have a `Functor` instance.

* The `pure` function does a simple and very boring thing: it embeds something into functorial (applicative) structure.
* The more core operation of this typeclas is `<*>`. This is an infix function called *apply* or sometimes *ap*, or *tie-fighter*.

In addition, `Control.Applicative` library provides some other convenient functions: `liftA`, `liftA2`, `liftA3`:

```
liftA  :: Applicative f => (a -> b)           -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

## Functor vs. Applicative

We notice that

```
(<$>) :: Functor     f =>   (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

The difference apperas to be quite small and innocuous. We now have `f` in front of the function `(a -> b)`. But the increase in power it introduces is profound. We can define a Functor in terms of a provided Applicative instance.

```
fmap f x = pure f <*> x
```

## Applicative functors are monoidal functors

First we notice that

```
 ($)  ::   (a -> b) ->   a ->   b
(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

And 

```
 <>   :: f          -> f   -> f
 ($)  ::   (a -> b) ->   a ->   b
(<*>) :: f (a -> b) -> f a -> f b
```

So in a sense, we're bolting a Monoid onto a Functor to be able to deal with functions embeded in additional structure. In another sense, we're enriching function application with the very structure we were previously merely mapping over with Functor. 

```
  [(*2),(*3)] <*> [4,5]
= [2*4, 2*5, 3*4, 3*5]
= [8, 10, 12, 15]
```
