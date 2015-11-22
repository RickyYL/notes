# Applicative

## TODO

* **17.7** QuickCheck
* **17.8** ZipList Monoid

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

De facto, `liftA` is just `fmap`, and `liftA2` and `liftA3` are just `fmap` varation with more arguments. 

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
In addition, `pure` is used to embed a value of any type in the structure. 

```
> pure 1 :: Either a Int
Right 1
> pure 1 :: ([a], Int)
([], 1)
```

The thing we notice is that the letf type is handled differently from the right type. That's because the left type is part of the structure, and the structure is not transformed by the function application.

## Applicative functors are monoidal functors

First we notice that

```
 ($)  ::   (a -> b) ->   a ->   b
(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

 <>   :: f          -> f   -> f
 ($)  ::   (a -> b) ->   a ->   b
(<*>) :: f (a -> b) -> f a -> f b
```

So in a sense, we're bolting a Monoid onto a Functor to be able to deal with functions embeded in additional structure. In another sense, we're enriching function application with the very structure we were previously merely mapping over with Functor. 

```
  [(*2),(*3)] <*> [4,5]
= [2*4, 2*5, 3*4, 3*5]
= [8, 10, 12, 15]

  Just (*2) <*> Just 2
= Just 4
  Nothing <*> Just 2
= Nothing
```

With Maybe, the ordinary functor is mapping over the possibility of a value's nonexistence. With the Applicative, now the function per se also might not be provided. 

### Tuple Monoid and Applicative

```
instance (Monoid a, Monoid b) => Monoid (a,b) where
	mempty = (mempty, mempty)
	mappend (a,b) (a',b') = (mappend a a', mappend b b')

instance (Monoid a) => Applicative ((,) a) where
	pure x = (mempty, x)
	(u,f) <*> <v,x> = (mappend u v, f x)
```

### Maybe Monoid and Applicative

```
instance Monoid a => Monoid (Maybe a) where
	mempty = Just mempty
	mappend _ Nothing = Nothing
	mappend Nothing _ = Nothing
	mappend (Just a) (Just a') = Just (mappend a a')

instance Applicative Maybe where
	pure = Just
	Nothing <*> _ = Nothing
	_ <*> Nothing = Nothing
	Just f <*> Just a = Just (f a)
```

## Applicative in use

### What's the List Applicative do?

```
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: [a -> b] -> [a] -> [b]

pure  :: a -> f a
pure  :: a -> [a]
```

With the List Applicative, we are mapping a plurality of functions over a plurality of values. We can see how this makes sense given that:

```
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
listApply :: [a -> b] -> [a] -> [b]
listFmap  :: (a -> b) -> [a] -> [b]
```

The *f* structure that is wrapped around our function in the `listApply` function is a list per se. Therefore, `a -> b` from Functor has become a list of `a -> b`. 

Let's see the exmaple again:

```
> [(*2),(*3)] <*> [4,5]
= [2*4, 2*5, 3*4, 3*5]
= [8, 10, 12, 15]

> (,) <$> [1,2] <*> [3,4]
= [(1,),(2,)] <*> [3,4]
= [(1,3),(1,4),(2,3),(2,4)]
```

It maps each function value from the first list over the second list, applies the operations and retuns on list. This looks just like Cartesian products.

### Identity

The `Identity` type here is a way to introduce structure without changing the semantics of what you're doing. 

```
> const <$> [1,2,3] <*> [9,9,9]
[1,1,1,2,2,2,3,3,3]

> const <$> Identity [1,2,3] <*> Identity [9,9,9]
Identity [1,2,3]
```

Having the extra bit of strcture around our values lifts the `const` function, from mapping over the lists to mapping over the `Identity`. If *f* is the list, `const` applies to the values inside the list. If *f* is `Identity`, then `const` treats the lists in side the `Identity` structure as a single value, not structure containing values.

### Maybe Applicative

When *f* is Maybe, we're saying the function per se might not exist, because we're allowing the possibility of the function to be applied being a Nothing case.

## Applicative laws

* **Identity** - `pure id <*> v = v`
* **Composition** - `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
* **Homomorphism** - `pure f <*> pure x = pure (f x)`
* **Interchange** -  `u <*> pure y = pure ($ y) <*> u `

Every Applicative instance you write should ovey those four laws. This keeps your code composable and free of unpleasant surprises. 

## QuickCheck

## ZipList Monoid
