#Define Types and Typeclass

##定义新的数据类型

####标准库的 `Bool` 类

```
data Bool = False | True
```

* `data` 表示定义新的数据类型。
* 左端部分为类型名称，是**类型构造器**。
* 右端为**值构造器**，指定了该类型可能的值。
* 类型名与值构造器首字母必须大写。

####自己实现 `Shape` 类

```
data Shape = Circle    Float Float Float
           | Rectangle Float Float Float Float
           deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

* 本质上值构造器是一个返回某数据类型的函数：
```
> :t Circle
Circle :: Float -> Float -> Float -> Shape
> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```
* `Circle` 或 `Rectangle` 并不是类型名，只有 `Shape` 是类型名，这点在函数签名上有所体现。
*  再次强调，值构造器本质上是函数。


####优化 `Shape` 类

为了优化定义，我们增加一个中间数据类型 `Point`：

```
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

* `Point` 的值构造器名字和类型名相同。在一个类型只有一个值构造器时，重名很常见。

更多的 `Shape` 类函数：

```
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = (Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)))
```
####将图形导出到模块中

通过如下代码将上述所有定义导出到自定义模块：

```
module Shapes -- module name
( Point(..)   -- export all value constructors of type Point
, Shape(..)
, area
, nudge
) where
```

* 可以通过 `Shape` 不带括号，从而只导出类而不导出构造器，隐藏了内部实现，实现了更高的抽象。
* 对于简单的数据类型，导出构造器没问题。

##记录语法

通过记录语法，可以为类型字段提供名称：

```
data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     , height    :: Float
                     , phoneNumber :: String
                     , flavor    :: String
                     } deriving (Show)
```

* 这种方法的好处在于，可以直接通过字段取值。
* 如果自动实现 `Show` 类型类，则输出时会打印字段名及值。
* 构造时只需要列出字段名及值，而不需要关心顺序。

##类型参数和类型构造器

类型构造器可以取类型作为参数，产生新的类型（像是 C++ 的模板）。例如 `Maybe` 类：

```
data Maybe a = Nothing | Just a
```

* 这里 `a` 是一个类型参数，`Maybe` 也是因而是一个类型构造器。
* 列表类型可以是 `[a]`，但不可以是 `[]`，因为它也是个类型构造器。
* 需要注意的是，`Nothing` 类型为 `Maybe a`，是*多态的*，可以在需要的时候自动转型。
* 一般而言，在不关心容器内类型的时候使用类型参数。
* 永远不要在 `data` 声明中添加类型约束，这样会留下许多无谓的类型约束。

####实现一个向量类

为了支持不同的数值类型，例如 `Int`、`Integer` 和 `Double`，我们令 `Vector` 类型参数化：

```
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+m)

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector i j k) (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) m  = Vector (i*m) (j*m) (k*m)
```

* 并没有给 `data` 声明添加 `(Num a)` 的类型约束，因为即便如此，该给函数添加的约束依然要添加。

##派生实例

**类型类**就是定义了某些行为的接口，如果类型拥有相关行为，就可以实现为类型类的实例。最简单的方式就是通过 `deriving` 自动实现类型类。

常见的类型类包括：`Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`。

##类型别名

类型别名可以实现类型等价与互换，例如标准库的 `type String = [Char]`。

* `type` 并不是创造新类型，而是为一个既有类型提供别名。

####`Either` 类

```
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

* 它有两个值构造器，如果用了 `Left`，内容类型就是 `a`；用了 `Right`，内容类型就是 `b`。
* 比 `Maybe` 的优势在于可以表达更多信息。

##递归数据结构

####自定义列表类

```
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

data List a = Empty
            | Cons { listHead :: a
                   , listTail :: List a
                   }
            deriving (Show, Read, Eq, Ord)
```

* 第二个版本为记录语法形式，更能看清值构造器 `Cons` 的作用。
* 值构造器 `Cons` 有两个字段，第一个类型为 `a`，第二个类型为 `List a`，是递归类型。

我们还可以定义一些运算符，来完善列表类的定义

```
infixr 5 :-:
data List a = Empty
            | a :-: (List a)
            deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys      = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
```

* 这里需要注意模式匹配 `(x :-: xs)`，之所以能这么匹配是因为模式匹配本质上是值构造器的匹配。

####树

```
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
            deriving Show

-- build a single root tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) | x == a = Node x left right
                                 | x < a  = Node a (treeInsert x left) right
                                 | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right) | x == a = True
                               | x < a  = treeElem x left
                               | x > a  = treeElem x right
```

##类型类

* 类型类比较接近于接口。
* 一个类型类定义了某些行为（判断相等、比较顺序、枚举）。
* 拥有某行为的类型，就作为某类型类的实例。
* 类型类的行为通过函数的定义实现。
* 如果某个类型是一个类型类的实例，那么可以针对这一类型调用该类型类定义的所有函数。

### `Eq` 类型类

```
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

* 函数体的实现是可选的，必须提供的是类型声明。
* 随后给出的函数体是交叉递归的形式实现的。
* 类型类给出的函数类型并非世纪类型，例如 `(==)` 的实际类型为 `(Eq a) => a -> a -> Bool`。

#### 自己实现 `TrafficLight` 类

```
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```

* 在类型类的声明中，因为 `(==)` 和 `(/=)` 互相依赖，因而只需要在实例声明中覆盖其中一个即可。
* 这种风格被称为类型类的**最小完备定义**，为符合类型类的行为，必须实现的最少数几个函数。

接下来顺手实现其 `Show` 类型类：

```Haskell
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

###子类化

在标准库 `Num` 类型类的定义中：

```
class (Eq a) => Num a where
    ...
```

* 这里限制了 `a` 必须是 `Eq` 的实例。
* 若要将一个类型实现为 `Num` 的实例，必先将该类型实现为 `Eq` 实例。
* 子类化所唯一要做的就是在类声明中添加类约束。

###作为类型类实例的带参数类型

```
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

* `Maybe` 不是具体类型，`Maybe m` 才是具体类型，只不过 `m` 是类型变量。

###查询类型类

* `:info TypeClass` 可以列出类型类中定义的信息，以及所有实例类型。
* `:info Type` 可以列出某类的所有类型类。

##`Functor` 类型类

```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

* `Functor` 类型类表示可以映射的事物。
* `f` 不是具体类型，而是取一个参数的类型构造器。
* `fmap` 的两个参数：取一个参数类型与返回类型不同的函数，一个应用到某类型的函子值；`fmap` 的返回值：一个应用到另一个类型的函子值。

```
map :: (a -> b) -> [a] -> [b]

instance functor [] where
    fmap = map
```

* `map` 取一个参数类型与返回类型不同的函数，和一个某类型列表作为参数，返回另一个类型的列表。
* `map` 也是一个函子，它是仅处理列表的 `fmap`。
* 之所以没有写作 `instance functor [a] where`，是因为 `f` 必须是一个带单个参数的类型构造器。`[]` 即是这样一个东西。

##kind 与无名类型

```
ghci> :k Int
Int :: *
```

* 带参数的类型构造器最终都能生成具体类型。类型构造器也能部分应用。
* 类型的类型，称为 kind。
* `*` 表示该类型是一个具体类型，即不含任何类型参数的类型，只有具体类型才可以拥有值。
* 因此，`Functor` 期望的 kind 为 `* -> *`。
