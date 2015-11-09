#Define Types and Typeclass

##定义新的数据类型

标准库的 `Bool` 如下定义：

```
data Bool = False | True
```

* `data` 表示定义新的数据类型
* 左端部分为类型名称
* 右端为*值构造器*（value constructor），指定了该类型可能得值
* 类型名与值构造器首字母必须大写

接下来自己定义一个 `Shape` 类：

```
data Shape = Circle    Float Float Float
           | Rectangle Float Float Float Float
           deriving (Show)

area :: Shape -> Float
area (Circle _ _ r)          = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

* 本质上值构造器是一个返回某数据类型的函数：
```
> :t Circle
Circle :: Float -> Float -> Float -> Shape
> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```
* `Circle` 或 `Rectangle` 并不是类型名，只有 `Shape` 是类型名，这点在函数签名上有所体现
*  再次强调，值构造器本质上是函数



















