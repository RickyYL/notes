# Types

## What are types?

Types ar ehow we group a set of values together that share something in common. 

Data constructors are the values of a particular type; they are also functions that let us create data, or values, of a particular type.

In Haskell, you cannot create untyped data. 

## Querying and Reading Types

```
fst :: (a,b) ->   a
--      [1]  [2] [3]
```

* **[1]** - Argument to `fst`, has the type `(a,b)`. The tuple per se `(,)` takes two arguments.
* **[2]** - The function type, `(->)`. One is argument `(a,b)` and one is the result `a`. 
* **[3]** - The result of the function. The same `a` that was in the tuple `(a,b)`.

## Typeclass-constrained type variables

## Currying

In Haskell, functions take one argument and return one result. Instead there are syntatic conveniences that construct curried functions by default. 

The arrows we've seen denote the function type. Each arrow represents one argument and one result., with the final type being the final result.

```
(+) :: Num a => a -> a -> a
--     [  1  ] [   2   ] [3]
```

* **[1]** - Typeclass constraint saying that `a` must have an instance of `Num`.
* **[2]** - Currying functions take two arguments one by one.
* **[3]** - The result type of the function.

The ability to apply only some of a function's arguments is described as *partial application*.

## Polymorphism

Broadly speaking, type signatures may have three kinds of types:
* **Concrete**
* **Constrained (ad hoc) polymorphic** - implemented with typeclasses.
* **Parametrically polymorphic** - refers to type variables. e.g. `id :: a -> a`.

## Type inference

Haskell's type inference is built on an extened version of the Damas-Hindley-Milner type system. 

## Asserting types for declarations
