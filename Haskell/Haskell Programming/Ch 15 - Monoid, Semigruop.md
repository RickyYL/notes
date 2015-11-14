# Monoid, Semigruop

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
* **Associatity**, means arguments can be regruoped in different orders and give the same result/
* **Identity**, means there exists some unit values.

`Monoid` is the typeclass that generalizes these laws across types.

## How Monoid is defined in Haskell

