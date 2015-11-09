#Define Types and Typeclass

##定义新的数据类型

####标准库的 `Bool` 类

```
data Bool = False | True
```

* `data` 表示定义新的数据类型。
* 左端部分为类型名称。
* 右端为*值构造器*（value constructor），指定了该类型可能的值。
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










