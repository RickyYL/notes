# Monoid, Semigruop

## TODO

* **15.10** Reusing algebras by asking for algebras
	* `Optional` type
	* The problem of orphan instances
* **15.11** Better living through QuickCheck
* Chapter Exercises


## What we talk about when we talk about algebras

Algebra means the study of mathematical symbols and the rules governing their manipulation.

In Haskell, these algebras can be implemented with typeclasses; the typeclasses define the set of operations. 

## Monoid

```
A monoid is a binary associative operation with an identity.
   [1]         [2]       [3]        [4]              [5]
```

* **[1]** - The thing we're talking about - monoids. That will end up being the name of our typeclass.
* **[2]** - Means, there will be two of something. 
* **[3]** - This is a property or law that must be satisfied.
* **[4]** - So called because in mathematics, it's usually used as an infix operator, or function. Hence, it's a binary function.
* **[5]** - In this context, we can take this to mean there'll be some value which, when combined with any other value, will always return that other value. 

e.g.

```
-- [] = mempty, or the identity. 
-- mappend is the binary operator to append two arguments.

mappend [1..5] [] = [1..5]
mappend [] [1..5] = [1..5]

-- more generally

mappend x mempty = x
mappend mempty x = x
```

So in plain English, a monoid is a function that takes two arguments and follows two laws:
* **Associatity** - means arguments can be regruoped in different orders and give the same result.
* **Identity** - means there exists some unit values.

`Monoid` is the typeclass that generalizes these laws across types.

### Examples

`((*), 1)`, `((+), 0)`, `(max, minBound)`, `((.), id)`, ...

## How Monoid is defined in Haskell

Typeclasses give us a way to recognize, organize, and use common functionalities and patterns across types that differ in some ways but also have things in common.

The `Monoid` typeclass recognizes and orders a different pattern than `Num` but the goal is similar. The typeclass merely abstracts that pattern out and makes it general and available to other datatypes that don't have predefined functions that do this for you (e.g. `(++)`), including your own datatypes when appropriate.

The typeclass `Monoid` is defined:

```
class Monoid m where
	mempty  :: m
	mappend :: m -> m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty
```

* `mappend` is how any two values that inhabit your type can be joined together.
* `mempty` is the identity value for that `mappend` operation.

## Examples of using Monoid

Remember, this typeclass just abstracts the pattern out, giving you the ability to use the operations over a larger range of types.

### List

The definition of `Monoid` for list is

```
instance Monoid [a] where
	mempty  = []
	mappend = (++)
```

## Why Integer doesn't have a Monoid

For integers, in mathematics, both summation and multiplication are monoidal, but each type should only have one unique instance for a given typelcass, not two.

Integers form a monoid under summation and multiplication. Similarly we can say that lists form a monoid under concatenation. Lists have more than one possible monoid, although for now we're only working with concatenation. Several other types do as well. We usually enforce the unique instance rule by using `newtype` to seprate the different monodial behaviors. 

### Why newtype?

First, there's not much semantic difference between the following datatypes:

```
data Server = Server String
newtype Server' = Server' String
```

The main differences are that using `newtype` *constains* the datatype to having a single unary data constructor and `newtype` guarantees no additional runtiome overhead in wrapping the original type. 

In summary, why using `newtype`:

* **Signal intent** - Make it clear that you only intend for it to be a wrapper for the underlying type.
* **Improve type safety** - Avoid mixing up many values of the same representation.
* Add different typeclass instances to a type that is otherwise unchanged representationally.

### More on Sum and Product

There's more than one valid `Monoid` instance one can write for numbers, so we use `newtype` wrappers to distinguish which we want. Importing `Data.Monoid` you'll see the `Sum` and `Product` data constructors.

```
> :t (<>)
(<>) :: Monoid m => m -> m -> m

> (Sum 8) <> (Sum 10)
Sum {getSum = 18}

> (Sum 8) <> (Sum 10) <> (Sum 10)
Sum {getSum = 28}

> mconcat [(Sum 1),(Sum 2),(Sum 3)]
Sum {getSum = 6}
```

## Why bother?

Because monoids are really common and the're a nice abstraction to work with when you have multiple monoidal things running around in a project. Knowing what a monoid is can help you to recognize when you've encountered the pattern.

When we say something *is* a Monoid or can be described as monoidal, we mean you can define at elast one law-abiding `Monoid` instance for it.

A common use of monoids is to structrue and describe common modes of processing data.

## Laws

`Monoid` instances must abide by the following laws:

* **[Left Identity]** - `mappend mempty x = x`
* **[Right Identity]** - `mappend x mempty = x`
* **[Associativity]**
	* `mappend x (mappend y z) = mappend (mappend x y) z`
	* `mconcat = foldr mappend mempty`

## Different typeclass instance, same representation

Monoid is somewhat differernt from other typeclasses in Haskell, in that many datatypes have more than one valid monoid. When we have more than one potential implementation for Monoid for a datatype, it's most convenient to use newtypes to tell them apart.

For some other datatypes the meaning of append is less clear. In these cases, the monoidal operation is less about combining values and more about finding a summary value for the set. Mappending is perhaps best thought of not as a way of combining values in the way that addition or list concatenation does, but as a way to condense any set of values to a summary value.

Boolean values have two possible monoids:
* `All`, a monoid of conjunction. 
* `Any`, a monoid of disjunction.

The `Maybe` type actually has more than two possible Monoids:
* `First`, find the first non-Nothing.
* `Last`, find the last non-Nothing.

## Reusing algebras by asking for algebras

De facto, `Maybe` type has another `Monoid` that combines the `a` values contained within the `Maybe a` type.

First to notice a pattern:

```
instance Monoid b => Monoid (a -> b)

instance (Monoid a, Monoid b) => Monoid (a,b)
instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c)
```

What these Monoids have in common is that they are giving you a new Monoid for a larger type by reusing the Monoid instances of types that represent components of the larger type.

### Associativity

Associatity simply says that you can associate the arguments of your operation differently and the result will be the same, whereas it is not as strong a property as an operation that commutes. Commutative means you can reorder the arguments, not just reassociate the parentheses, and still get the same result.

Commutativity is a strong property and can be useful in circumstances when you might need to be able to reorder evaluation of your data for efficiency purposes without needing worry about the result changing.

### Identity

An identity is a value with a special relationship wth an operation: it turns the operation into the identity function. 

Zero is the identity value for addition, while one is the identity value for multiplication. It doesn't make sense to talk about zero and one as "identity values" outside the context of those operations.

### The problem of orphan instances

TODO

## Better living through QuickCheck

TODO

## Semigroup

For a `Semigroup`, the core operation remains binary and associative, but doesn't have an identity value. In that sense, it's a weaker algebra.

```
class Semigroup a where
	(<>) :: a -> a -> a
```

And we're left with one law: `(a <> b) <> c == a <> (b <> c)`.

Before using `Semigroup` typeclass, install it via `cabal`.

### NonEmpty, a useful datatype

`NonEmpty` list type is oen really useful datatype that can't have a `Monoid` instance but does have a `Semigroup` instance. It's a list datatype that can never be an empty: 

```
data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)
```

We cannot write a Monoid for `NonEmpty` list because it has no identity value by design.

### Strength can be wekness

When Haskellers talk about the *strength* of an algebra, they usually mean the number of operations it provides which in turn expands what you can do with any given instance of that algebra without needing to know specifically what type you are working with.

The most obvious way to see that `Monoid` is *stronger* than `Semigroup` is to observe that it has a strict superset of the operations and laws that `Semigroup` procides. Anything which is a `Monoid` is by definition also a semigroup.

As we can see the increasing methods we can do for something, the stricter constrints it limits us:

```
id :: a -> a
inc :: Num a => a -> a
somethingInt :: Int -> Int
```

When `Monoid` is too strong or more than we need, we can use `Semigroup`. The even weaker typeclass is `Megma` which removes the associativity requirement.
