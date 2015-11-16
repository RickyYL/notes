# Define Typeclass

## 定义类型类

类型类使用 `class` 关键字定义，比如：

```
Class Typeclass_name a where
	fun1 = ...
	fun2 = ...
```

比如 `Eq` 类型类是这样定义的：

```
class Eq a where
	(==) :: a -> a -> Bool
	(/=) :: a -> a -> Bool
	x == y = not (x /= y)
	x /= y = not (x == y)
```

我们可以自己定义一个类，并实现其类型类：

```
data MyNum = O | Zero | One

instance Eq MyNum where
	O    == Zero = True
	O    == O    = True
	Zero == Zero = True
	One  == One  = True
	_    == _    = False
```

定义类型类时，通过与类型类的约束，可以表达类型类间的依赖关系：

```
class (Eq a) => Ord a where
	...
```

## Haskell 中常见的类型类

### 常见类型类

以下是 `Ord` 类型类的定义：

```
class (Eq a) => Ord a where

    compare           :: a -> a -> Ordering
    (<),(<=),(>),(>=) :: a -> a -> Bool
    max, min          :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT

    x < y = case compare x y of { LT -> True; _  -> False; }
    ...

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
```

以下是 `Bounded` 类型类的定义：

```
class Bounded a where

    minBound :: a
    maxBound :: a
```

以下是 `Enum` 类型类的定义：

```
class Enum a where

    toEnum   :: Int -> a
    fromEnum :: a -> Int

    succ, pred     :: a -> a
    enumFrom       :: a -> [a]           -- [n..]
    enumFromThen   :: a -> a -> [a]      -- [n, n'..]
    enumFromTo     :: a -> a -> [a]      -- [n..m]
    enumFromThenTo :: a -> a -> a -> [a] -- [n,n'..m]
```

为了将一个类以 `String` 的形式输出，需要实现 `Show` 类型类，例如：

```
data MyNum = One | Two | Three

instance Show MyNum where

    show One   = "1"
    show Two   = "2"
    show Three = "3"
```

### `Functor` 类型类

`Functor` 类型类定义如下：

```
import Data.Functor

class Functor f where
	fmap :: (a -> b) -> f a -> f b

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

本质上，`Functor` 意为给定类型 `a` 与类型 `b` 之间存在一个映射，`Functor` 可以返回另一个参数化类型上的映射。这个参数化类型的 kind 必须是 `* -> *`。

有了 `Functor` 类型类，我们可以通过一个函数将容器内的一种类型的值，映射为另一种类型的值。

比如，我们可以将 `[a]` 通过一个类型为 `a -> b` 的函数映射成 `[b]`。`Functor` 就是对其他容器这一性质的抽象，并且为 `fmap` 定义了运算符 `<$>`。我们可以很方便的为自定义类型实现 `Functor` 类型类，从而使用 `fmap`：

```
newtype Container a = Container a

instance Functor Container where
    fmap f (Container a) = Container (f a)

instance Functor [] where
    fmap = map 

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)
```

### `Functor` 定律

* 单位元：`fmap id = id`
* 分配率：`fmap (f . g) = fmap f . fmap g`

### `Applicative` 类型类

在 `Functor` 类型类的基础上，我们需要一种*更为一般地把任意函数应用到任意类型的构造器内的值*。这正是 `Applicative` 类型类所做的。

```
import Control.Applicative

class Functor f => Applicative f where
    pure  :: a -> f a                   -- 封装一个函数
    (<*>) :: f (a -> b) -> f a -> f b   -- 将封装的函数取应用到封装的值上
```

以下是 `Maybe` 类实现 `Applicative` 类型类的声明：

```
instance Applicative Maybe where
    pure             = Just
    Nothing  <*> _   = Nothing
    (Just f) <*> arg = fmap f arg
```

如此，我们便能实现如下操作：

```
> :t Just (+)
Just (+) :: Num a => Maybe (a -> a -> a)

> Just (+) <*> Just 1 <*> Just 2
Just 2

-- Derivation

  Just (+) <*> Just 1 <*> Just 2
= (fmap (+) Just 1) <*> Just 2
= Just (1+) <*> Just 2
= fmap (1+) Just 2
= Just 3
```

一组对比：

```
($)   ::   (a -> b) ->   a ->   b
(<$>) ::   (a -> b) -> f a -> f b    -- Functor
(<*>) :: f (a -> b) -> f a -> f b    -- Applicative
```

`Applicative` 除了具有 `Functor` 的特性以外，能做的只是调用函子容器内的函数。因此也常称之为 `Applicative Functor`。为了更方便的地实现这个特性，GHC 库中实现了一组函数，将一个函数运算的参数分别放置于实现了 `Applicative` 类型类的容器中：

```
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b         -- liftA2 f a b = pure f <*> a <*> b
```

除此之外，`Applicative` 还定义了另外两个运算符：

```
(*>) :: f a -> f b -> f b
(*>) = liftA2 (const id)    -- 先计算第一个参数并忽略其结果，计算第二个参数并返回

(<*) :: f a -> f b -> f a
(<*) = liftA2 const         -- 相反
```

列表也实现了 `Applicative` 类型类，行为与列表生成差不多：

```
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [ f x | f <- fs, x <- xs ]
```

`((->) r)` 也可以实现为 `Applicative`：

```
instance Applicative ((->) r) where

    pure :: a -> (r -> a)
    pure x _ = x
    
    (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
    (<*>) f g x = (f . g) x
```

### `Applicative` 定律

* 单位元：`pure id <*> v = v`
* 复合定律：`pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
* 同态定律：`pure f <*> pure x = pure (f x)`
* 互换定律：`u <*> pure y = pure ($ y) <*> u`

### `Alternative` 类型类

定义如下：

```
infixl 3 <|>

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a        
```

该运算符可以被用来在一些数据中选择合理的值，根据不同的数据类型行为有所区分，例如：

```
> Nothing <|> Nothing <|> Just 1 <|> Just 2
Just 1
```

`Maybe` 的实现如下：

```
instance Alternative Maybe where
    empty         = Nothing
    Nothing <|> p = p
    Just x <|> _  = Just x
```

列表的实现如下：

```
instance Alternative [] where
    empty = []
    (<|>) = (++)
```

`Applicative` 还定义了 `some` 与 `many` 函数，定义如下：

```
-- one or more
some :: f a -> f [a]
some f = (:) <$> f <*> many f

-- zero or more
many :: f a -> f [a]
many f = some f <|> pure []  
```

还有一个 `optional` 函数：

```
optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
```