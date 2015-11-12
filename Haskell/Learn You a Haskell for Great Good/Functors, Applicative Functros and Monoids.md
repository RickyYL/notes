# Functors, Applicative Functors and Monoids

在 Haskell 中，我们不需要把类型之间的关系想象为一个层级结构。相反，我们思考类型的行为是什么，通过类型类将其关联起来。

## Functors

`Functor` 是可以被映射的东西。`fmap` 意为：给我一个【取参数 `a`，返回 `b` 的函数】，以及一个【装有 `a` 的容器】，我会返回一个【装有 `b` 的容器】。`fmap` 在保持上下文不变的条件下，把函数应用到值上。

为了将一个类实现 `Functor` 类型类，其 kind 必须是 `* -> *`，表示取一个具体类型作为类型参数。

### 作为 `Functor` 的 IO 操作

IO 操作是一个 `Functor`，实现如下：

```
instance Functor IO where
	fmap f action = do
		result <- action
		return (f result)
```

如果某个值的类型是 `IO String`，那表明其是一个 `IO` 操作，会从外部世界为我们获取字符串。可以在 `do` 语法中使用 `<-` 将其结果赋予一个变量。

在一个 `IO` 操作上应用一个函数的结果还是一个 `IO` 函数，所以能用 `do` 语法来把两个 `IO` 操作粘合在一起形成一个新的 `IO` 操作。

`return` 创建了一个 `IO` 操作：只做一件事，把某个值作为结果。

由于 `do` 代码块产生的 `IO` 操作总是返回最后一个 `IO` 操作的结果，这就是为什么我们用 `return` 来创建一个什么都不做只返回结果的 `IO` 操作，即作为总体的结果返回。

如果需要把 `IO` 操作的结果赋予某一变量，对其应用一些函数，并把结果赋予另外一个变量，那么这种情况下就应该考虑使用 `fmap` 改写。例如：

```
import Data.Char
import Data.List

main = do Line <- fmap (intersperse '-' . reverse . map toUpper) getLine
	      putStrLn line
```

###作为 `Functor` 的函数

`(->) r` 也是一个 `Functor`，实现如下：

```
import Control.Monad.Instances

instance Functor ((->) r) where
	fmap f g ::（a -> b) -> (r -> a) -> (r -> b)
	fmap f g = f $ g
```

这表明，在一个函数上映射一个函数，结果肯定是一个函数。它的类型告诉我们，它接受一个 `(a->b)` 的函数，和一个 `(r->a)` 的函数，返回一个 `(r->b)` 的函数。这与函数组合运算符 `(.)` 的作用是一样的。很明显，对函数做 `fmap` 就是函数组合。

这一事实改变了我们的想法：那些表现得更像是计算而不是容器的东西，也是函子。

## `Functor` 定律

* 定律一：`fmap id = id`。
* 定律二：`fmap (f.g) = fmap f . fmap g`。

如果我们确认某类型遵守这两条定律，就能认为对于映射，它会有相同的基本行为。

###违反定律

```
data CMaybe a = CNothing | CJust Int a deriving Show

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)
```

`CMaybe` 类并不遵守函子定律，虽然 Haskell 并不强制要求遵守函子定律，但是这会导致使用中存在问题。函子应当只进行映射，而不干其他事情，这会让我们的代码更加抽象，更容易扩展。

## `Applicative` 类型类

```
import Control.Applicative

class Functor f => Applicative f where
	pure  :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```

`pure` 是一个接受任意类型值，并返回一个包裹了该值的 `applicative` 值的一元函数。或者说，它接受一个值，把它放在某个默认的上下文中——生成带有同样值的最小上下文。

### `Maybe Applicative Functor`

实现如下：

```
instance Applicative Maybe where
	pure 	         = Just
	Nothing  <*> _   = Nothing
	(Just f) <*> arg = fmap f arg
```

从中我们可以看出如下几点：
* `f` 是一个单类型参数的 `Applicative Functor`。
* `pure` 的作用就是把任意值包裹为 `Applicative` 值，这里包裹函数就是 `Just` 值构造器。
* `<*>` 有两条规则：
  * 我们不能从 `Nothing` 中取出函数，因为它里面没有函数。
  * 如果第一个参数不是 `Nothing`，那么就从中取出函子，应用到右边的值上。

### `Applicative` 风格

`Control.Applicative` 中 `fmap` 运算符：

```
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

如果我们想把函数 `f` 应用到三个 `Applicative` 值上，可以写成 `f <$> x <*> y <*> z`。如果参数是普通值的话，就写成 `f x y z`。例如：

```
> (++) "Ricky" "Lee"
"RickyLee"

> (++) <$> Just "Ricky" <*> Just "Lee"
Just "RickyLee"
```

###列表也是 `Applicative`

列表的 `Applicative` 实现：

```
instance Applicative [] where
	pure x = [x]
	fs <*> xs = [f x | f <- fs, x <- xs]
```

用 `Applicative` 风格实现列表处理，往往是 list comprehenion 不错的替代品。例如：

```
> [x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]

> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
```

风格上的不同见仁见智。

### IO 也是 `Applicative`

IO 的实现如下：

```
instance Applicative IO where
	pure    = return
	a <*> b = do f <- a
                 x <- b
                 return (f x)
```

考虑如下一段代码：

```
myAction :: IO String
myAction = do a <- getLine
              b <- getLine
			  return $ a ++ b
```

等价为

```
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
```

###函数也是 `Applicative`

函数 `((->) r)` 的实现如下：

```
instance Applicative ((->) r) where
	pure x  = (\_ -> x)
	f <*> g = \x -> (f . g) x
```

考虑如下几个例子：

```
> pure 3 "blah"
3

> (+) <$> (+3) <*> (*100) $ 5
508

> (\x y z -> [x,y,z]) <$> (+3) <*> (*10) <*> (/2) $ 5
[8.0,50.0,2.5]
```

### `zip` 列表

之前我们看到列表的 `Applicative` 表现为笛卡尔乘积，如果想做加法的话，可以使用 `ZipList`，定义如下：

```
import Control.Applicative

instance Applicative ZipList where
	pure x = ZipList (repeat x)
	ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

## `Applicative` 定律

* `pure f <*> x = fmap f x`
* `pure id <*> v = v`
* `pure (.) <*> x <*> v <*> w = u <*> (v <*> w)`
* `pure f <*> pure x = pure (f x)`
* `u <*> pure y = pure ($ y) <*> u`

## `Applicative` 的实用函数

### `liftA2`

```
import Control.Applicative

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
```

对 `liftA2` 类型的理解：

* `(a -> b -> c)`：接受一个二元函数为参数。这个二元函数接受两个普通值，返回另一个。
* `(f a -> f b -> f c)`：返回一个二元函数。这个二元函数接受两个函子，返回另一个函子。

看一个实例：

```
> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
```

* `liftA2 (:)`：成为了一个二元函数 `(f a -> f b -> f c)`，接受两个函子为参数，返回一个函子。
* `(Just 3) (Just [4])`：为二元函数 `liftA2 (:)` 的两个函子参数。
* `Just [3,4]`：返回的函子。

###`sequenceA`

```
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- another definition for sequenceA

sequenceA = foldr (liftA2 (:)) (pure [])
```

`sequenceA` 接受任意数目的 applicetive 值，合并成为一个 applicative 值。