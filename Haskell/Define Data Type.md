#Define Data Type

定义新类型主要是 `data` 与 `newtype` 关键字。

##数据类型定义

###枚举类型

```
data Bool = False | True
data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq, Ord, Enum, Read)

> Mon
Mon
> Mon == Mon
True
> Mon < Sun
True
> [Mon .. Sun]
[Mon,Tue,Wed,Thu,Fri,Sat,Sun]
> Read "Mon" :: Weekday
Mon

tomorrow Sun = Mon
tomorrow day = succ day

yesterday Mon = Sun
yesterday day = pred d
```

通过 `:info typename` 可以查询一个类型是如何定义的，以及其实现了哪些相应的类。

另外，有时会需要用到类型 `()`，称为*单位类型*，只有一个值 `()`。

###构造类型

```
type Name   = String
type Author = String
type ISBN   = String
type Price  = Float
data Book   = Book Name Author ISBN Price deriving (Show, Eq)
```

其中，第一个 `Book` 是类型名字，称为类型构造器，但没有参数。第二个 `Book` 称为数据构造器。此外，为了方便访问，还可以如下定义：

```
data Book = Book {
	name   :: Name,
	author :: Author,
	isbn   :: ISBN,
	price  :: Price
}
```

###参数化类型

参数化类型是一些需要类型参数的类型。其定义中的类型名称后可以加任意一个类型参数以供这个类型的数据构造器使用。这里的参数可以是任何多态类型，而不是一个确定的类型。

```
data Maybe a = Nothing | Just a
```

`Maybe` 的意义在于，它是一种处理程序异常的方式。有时候，程序需要带着出现的异常继续运行，而不是简单的终止，异常出现时，通过返回 `Nothing` 报告异常。

```
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv a 0 = Nothing
safeDiv a b = Just (div a b)
```

####类型构造器

例如，`Maybe Bool` 是一个类型，它有三个值：`Nothing`, `Just True` 和 `Just False`。像 `Maybe` 这样需要其他类型作为参数构造一个新的类型的类型，称为*类型构造器*。


























