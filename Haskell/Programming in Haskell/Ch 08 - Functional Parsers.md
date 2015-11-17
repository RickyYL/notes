# Functional Parser

## Parsers

A **Parser** is a program that takes a string of characters, and produces some form of tree that makes the syntactic structure of the string explicit. 

## The parser type

In Haskell, a parser can naturally be viewed directly as a function that takes a string and produces a tree. Hence,

```
Parser :: String -> Tree
```

In general, however, a parser might not always consume its entire argument string. We generalise our type for parsers to also return any unconsumed part of the argument string:

```
Parser :: String -> (Tree, String)
```

Similarly, a parser might not always succeed. We generalise our type for parsers to return a `Maybe (Tree, String)` data, to indicate that it may fail:

```
Parser :: String -> Maybe (Tree, String)
```

To enable parsers to return any kind of values, we change it into:

```
Parser a :: String -> Maybe (a, String)
```

In summary, this declaration states that a parser of type `a` is a function that takes an input string and produces a list of result, each of which is a pair comprising a result value of type `a` and an output string.

## Basic parsers

```
return :: a -> Parser a
return v =  \ input -> Just (v, input)

failure :: Parser a
failure = \ input -> Nothing

item :: Parser Char
item = \ input -> case input of 
                  []     -> Nothing
				  (x:xs) -> Just (x, xs)
```

* `return v` - Always succeeds with the result value `v`, without consuming any of the input string.
* `failure` - Always fails, regardless of the contents of the input string.
* `item` - Fails if the input string is empty, and succeeds with the first character as the result value otherwise. The `case` machanism of Haskell used in this definition allows pattern matching to be used in the body of a definition. 

Because `Parser`s are functions, they could be applied to a string using normal function application.

```
parse :: Parser a -> String -> Maybe (a, String)
parse p input = p input
```

## Sequencing

Perhaps the simplest way of combining two parsers is to apply one after the other in sequence, with the output string returned by the first parser becoming the input string to the second. 

In practice, it turns out to be more convenient to combine sequencing of parsers the the processing of their result values, by means of sequencing operator `>>=` (read as then) defined as follows:

```
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \ input -> case parse p input of 
                     Nothing      -> Nothing
					 Just (v,out) -> parse (f v) out
```