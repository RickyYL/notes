# Strings

## A first look at types

Types are a way of categorizing values.

## Printing simple strings

```
module Print1 where

main :: IO ()
main = putStrLn "Hello, World!"
```

```
module print2 where

main :: IO ()
main = do
	putStrLn "Count to four for me: "
	putStr   "one, two"
	putStr   ", three, and"
	putStrLn " four!"
```

### Global vs. local definitions

To be locally defined would mean the declaration is nested whitin some other expression and is not visible to code importing the module. 

## Type signatures of concatenation functions

```
(++)   :: [a] -> [a] -> [a]
concat :: [[a]] -> [a]
```

```
(++) :: [a] -> [a] -> [a]
--      [1]    [2]    [3]
```

Everything after `::` is about types. The `a` inside the list type constructor `[]` is a type variable.

* Take an argument of tpe `[a]`, which is a list of elements whose type we don't yet know.
* Take another argument of type `[a]`, a list of elements whose type we don't know. Because the variables are the same, they must be the same type throughout `(a==a)`.
* Return a result type of `[a]`.

The type variable `a` in `[a]` is polymorphic.
