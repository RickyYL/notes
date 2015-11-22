# Monad

## TODO

* **18.5** QuickCheck Part
* **18.6** Application and composition

> There is nothing so practical as a good theory. -- Phil Wadler, quoting Kurt Lewin

Finally we come to one of the most talked about structures in Haskell: the monad. Monads are not, strickly speaking, necessary to Haskell.

## What is Mooad

Monads are *applicative functors* with some unique features that make it a bit more powerful than either alone.
* A functor maps a function over some structure.
* An applicative maps a function that is contained over some structure over some structure and then mappends the two bits of structure. 

```
class Applicative m => Monad m where
	(>>=)  :: m a -> (a -> m b) -> m b
	(>>)   :: m a -> m b -> m b
	return :: a -> m a
```

### Applicative m

Older versions of GHC didn't have Applicative as a superclass of Monad. Given that Monad is stronger than Applicative, and Applicative is tronger than Functor, you can derive Applicative and Functor in terms of Monad. 

```
fmap f xs = xs >>= return . f
```

But the true chain of dependency is

```
Functor -> Applicative -> Monad
```

### Core operations

`return` really does nothing than wrapping. 

`(>>)` usually called **sequencing operator**, sequencing two actions while discarding any resulting value of the first action.

`(>>=)` is called **bind** and contains the things that are special about `Monad`.

### The noval part of Monad

```
<$> :: Functor f     =   (a -> b) -> f a        -> f b
<*> :: Applicative f = f (a -> b) -> f a        -> f b
>>= :: Monad f       = f a        -> (a -> f b) -> f b
```

Conventionally when we use monads, we use the bind function, `(>>=)`. Sometimes we use it directly, sometimes indirectly via `do` syntax. De facto, `bind` is quite similar to `<*>` and `fmap` but with the first two arguments flipped. Still, the idea of mapping a function over a value while bypassing its surrounding structure is not unique to `Monad`. 

```
> let addOne x = [x, 1]

> addOne 10
[10,1]

> fmap addOne [4,5,6]
[[4,1],[5,1],[6,1]]

> concat $ fmap addOne [4,5,6]
[4,1,5,1,6,1]
```

In a sense, `Monad` is a generalization of `concat`. The unique part of `Monad` is the following function:

```
import Control.Monad (join)
join :: Monad m => m (m a) -> m a

-- compare

concat :: [[a]] -> [a]
```

Allowing the function per se to alter the struture is something we've not seen in `Functor` and `Applicative`, and we'll explore the ramifications of that ability more. We can inject more structure with a standard `fmap` if we wish. The ability to flatten those two layers of structure into one is what truly makes `Monad` special. We can simply define `bind` in terms of `fmap` and `join`.

```
bind :: Monad m => (a -> m b) -> m a -> m b
bind' f a = join $ fmap f a
```

### What Monad is not

* Impure. Monadic functions are pure functions.
* An embedded language for imperative programming.
* A value. The typeclass describes a specific relationship between elements in a domain and defines some operations over them.
* About strictness. The monadic operations of `bind` and `return` are nonstrict Some operations can be made strict within a specific instance. 

Using monads also doesn't require knowing math, specificly, category theory. 

### Monad also lifts

The `Monad` class also includes a set of `lift` functions that are the same as the ones we already saw in `Applicative`. They don't really do anything different, but they are still around because some libraries used them before applicatives were discovered, so the `liftM` set of functions still exists to maintain compatibility. 

```
liftA :: Applicative f => (a -> b) -> f a -> f b
liftM :: Monad m       => (a -> r) -> m a -> m r
```

## Do syntax and monads

```
(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m       => m a -> m b -> m b
```

For our purposes, `(*>)` and `(>>)` are the same thing: sequencing functions, but with two different constraints. They should in all cases do the same thing.

See the following example:

```
sequencing :: IO ()
sequencing = do 
    putStrLn "Hello"
    putStrLn "World"

sequencing' :: IO ()
sequencing' =
    putStrLn "Hello" >>
    putStrLn "World"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "Hello" *>
    putStrLn "World"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn
```

### When fmap alone isn't enough

Note that if you try to `fmap purStrLn` over `getLine`, it won't do anything.

```
> putStrLn <$> getLine
Hello world!
> -- [waiting]
```

First, `getLine` performs `IO` to get a `String`:

```
getLine :: IO String
```

And `putStrLn` takes a `String` argument, performs `IO`, and returns nothing interesting. 

```
putStrLn :: String -> IO ()
```

What's the type of `fmap putStrLn getLine`?

```
<$> :: Functor f => (a -> b) -> f a -> f b

putStrLn :: String -> IO ()
--          (a     -> b   )
```

That means, `b` has type `IO ()`, Which is goint to jam `IO` action inside of the `IO` that `getLine` performs. So that what's happening with out types:

```
f :: Functor f = f String -> f (IO ())
f x = putStrLn <$> x

g :: (String -> b) -> (IO b)
g x = x <$> getLine

h :: IO (IO ())
h = putStrLn <$> getLine
```

What we need to do is to use `join` those two `IO` layers together.

```
join $ putStrLn <$> getLine :: IO ()
```

**Monad: it's got what coders crave.**

What `join` did here is merge the effects of `getLine` and `putStrLn` into a single `IO` action. This merged `IO` action performs the effects in the order determined by the nesting of the `IO` actions. As we said, monadic actions ar estill pure, and the sequencing operations we use here are just was of nesting lambdas. Though `IO` allows side effects, but since those effects are constrained within the `IO` type, all the rest of it is still a pure lambda calculus. 

## Examples of Monad use

### List 

```
(>>=) :: Moand m => m  a -> (a ->  m  b) ->  m  b
(>>=) ::           [ ] a -> (a -> [ ] b) -> [ ] b

return :: Monad m => a ->  m  a
return ::            a -> [ ] a
```

```
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    xs >>= \x -> if even x 
                 then [x*x, x*x] 
                 else [x*x]
```

### Maybe

This is what's actually in GHC's base library at time of writing.

```
instance Monad Maybe where
    return x       = Just x
    (Just x) >>= k = k x
    Nothing  >>= _ = Nothing
```

If your `do` syntax looks like this:

```
doSomething = do
    a <- f
    b <- g
    c <- h
    return (zed a b c)
```

You can rewrite it using Applicative. On the other hand, if you have something like this: 

```
doSomething = do
    a <- f
    b <- g
    c <- h
    zed a b c
```

You're going to need Monad because `zed` is producing more monadic structure, and you'll need join to crunch that back down.

* With the `Maybe Applicative`, each `Maybe` computation fails or succeeds independently of each other. You're just lifting functions that are also `Just` or `Nothing` over `Maybe` values.
* With the `Maybe Monad`, computations contributing to the final result can choose to return `Nothing` based on previous computations.

### Either

### Monad laws

* **Identity** - `m >>= return = m`, `return x >>= f = f x`
* **Associativity** - `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

## Application and composition
