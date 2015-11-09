#Define Types and Typeclass

##定义新的数据类型

####`Bool` 如下定义

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

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = (Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)))
```

* `Point` 的值构造器名字和类型名相同。在一个类型只有一个值构造器时，重名很常见。














