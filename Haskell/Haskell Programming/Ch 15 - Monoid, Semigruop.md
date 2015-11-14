# Monoid, Semigruop

## TODO

* **15.10** Reusing algebras by asking for algebras
	* Exercise
	* The problem of orphan instances
* **15.11** Better living through QuickCheck
***

This chapter will include:
* Algebras!
* Laws!
* Monoids!
* Semigruops!

## What we talk about when we talk about algebras

Algebra means the study of mathematical symbols and the rules governing their manipulation.

In Haskell, these algebras can be implemented with typeclasses; the typeclasses define the set of operations. 

## Monoid

```
A monoid is a binary associative operation with an identity.
   [1]         [2]       [3]        [4]              [5]
```

* **[Monoid]** - The thing we're talking about - monoids. That will end up being the name of our typeclass.
* **[Binary]** - Means, there will be two of something. 
* **[Associative]** - This is a property or law that must be satisfied.
* **[Operation]** - So called because in mathematics, it's usually used as an infix operator, or function. Hence, it's a binary function.
* **[Identity]** - In this context, we can take this to mean there'll be some value which, when combined with any other value, will always return that other value. 

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
* **Associatity**, means arguments can be regruoped in different orders and give the same result.
* **Identity**, means there exists some unit values.

`Monoid` is the typeclass that generalizes these laws across types.

### Examples

`((*), 1)`, `((+), 0)`, `(max, minBound)`, `((.), id)`, ...

## How Monoid is defined in Haskell

Typeclasses give us a way to recognize, organize, and use common functionalities and patterns across types that differ in some ways but also have things in common.

The `Monoid` typeclass recognizes and orders a different pattern than `Num` but the goal is similar. The typeclass merely abstracts that pattern out and makes it general and available to other datatypes taht don't have predefined functions taht do this for you (e.g. `(++)`), including your own datatypes when appropriate.

The typeclass `Monoid` is defined:

```
class Monoid m where
	mempty  :: m
	mappend :: m -> m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty
```

* `mappend` is how any two values that inhabit your type can be joined together.
* `mempty` is the identity value for taht `mappend` operation.

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

For integers, in mathematics, both summation and multiplication ar emonoidal, but each type should only have one unique instance for a given typelcass, not two.

Integers form a monoid under summation and multiplication. Similarly we can say taht lists form a monoid under concatenation. Lists have more than one possible monoid, although for now we're only working with concatenation. Several other types do as well. We usually enforce the unique instance rule by using `newtype` to seprate the different monodial behaviors. 

### Why newtype?

First, there's not much semantic difference between the following datatypes:

```
data Server = Server String
newtype Server' = Server' String
```

The main differences are that using `newtype` *constains* the datatype to having a single unary data constructor and `newtype` guarantees no additional runtiome overhead in wrapping the original type. 

In summary, why using `newtype`:

* **[Signal intent]** - make it clear that you only intend for it to be a wrapper for the underlying type.
* **[Improve type safety]** - avoid mixing up many values of the same representation.
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

We will now be concerned not with chossing one value out of a set of values but of combining the a values contained within the `Maybe a` type.

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

An orphan instane is when an instance is defined for a datatype and typelcass, but not in the same module as either the declaration of the typeclass or the datatype. If you don't own the typeclass or the datatype, newtype it. 

## Better living through QuickCheck

### Validating associativity with QuickCheck

```
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
acs (<>) a b c = a <> (b <> c) == (a <> b) <> c
```

```
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c
```

## Semigruop

