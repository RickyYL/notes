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

例如，`Maybe Bool` 是一个类型，它有三个值：`Nothing`, `Just True` 和 `Just False`。像 `Maybe` 这样需要其他类型作为参数构造一个新的类型的类型，称为*类型构造器*。根据类型构造器，可以将类型化为不同的 kind。类型构造器是基于类型的运算，所有类型通常记为 `*`，意为所有类型。这里类型构造器的类型就称为 kind。

另外一个重要的类型是 `Either`，需要两个不同的类型为输入。
```
data Either a b = Left a | Right b

> :k Either
Either :: * -> * -> *
```

例如，构造列表的时候，不能用不同类型，此时可以使用 `Either` 来存放两种类型。
```
> :t [Left 90, Right "Cheated", Left 95, Right "Illness"]
~ :: Num a => [Either a [Char]]
```

相比于 `Maybe` 类型，如果用 `Either`，我们不仅可以知道有异常发生，还能知道异常的一些具体信息。

当需要把 `Either` 中的值映射为另一个值时，要为 `Left` 和 `Right` 分别提供一个函数，这两个函数返回类型相同。
```
either :: (a->c) -> (b->c) -> Either a b -> c
either f _ (Left  x) = f x
either _ g (Right y) = g x
```

我们可以把两个列表合并，或者将一个列表拆开：
```
disjoint as bs = map Left as ++ map Right bs

partition = foldr (either left right) ([],[])
            where left  a (l, r) = (a:l, r)
                  right a (l, r) = (l, a:r)
```

下面定义一个 `Piar` 类：
```
data Pair a b = Pair a b
pfst (Pair a b) = a
psnd (Pair a b) = b
```

####函数类型

函数类型也是有类型构造器的，是 `(->)`，`a -> b` 也可以写作 `(->) a b`。

###递归类型

所谓递归类型，就是定义时用到了自身的类。

例如我们可以递归定义自然数，根据皮亚诺公理：
```
data Nat = Zero | Succ Nat (Show, Eq)

natToint :: Nat -> Int
natToint Zero = 0
natToint (Succ n) = 1 + natToint n

intTonat :: Int -> Nat
intTonat 0 = Zero
intTonat n = Succ (intTonat (n-1))
```

###杂合定义类型

```
data Shape = Circle Float
           | Rect Float Float
           deriving (Show, Eq)

data Shape = Circle {
             radius :: Float
         } | Rect {
             len    :: Float ,
             width  :: Float
         } deriving (Show, Eq)

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect a b) = a * b
```

```
data BoolExp = TRUE
             | FALSE
             | IF BoolExp BoolExp BoolExp

eval :: BoolExp -> Bool
eval TRUE  = True
eval FALSE = False
eval (IF con b1 b2) | eval con == True  = eval b1
                    | eval con == False = eval b2
```

```
> :i []
data [] a = [] | a : [a]
```

```
data List a = Nil a
            | Cons a (List a)
            deriving (Show, Eq)

lhead :: List a -> a
lhead (Nil a)     = a
lhead (Const a _) = a

listTomylist Nil         = []
listTomylist (Cons x xs) = x : (listTomylist xs)

mylistTolist []     = Nil
mylistTolist (x:xs) = Cons x (mylistTolist xs)
```

##类型的同构 Isomorphism

         +----<----+
     +->-+    g    +->-+
id_A | A |         | B | id_B
     +-<-+    f    +-<-+
         +---->----+

如上两个类型 A B 为同构。记为 A ~= B。

##使用 `newtype` 定义新类型

`newtype` 只能定义单一构造器，并且该构造器只能有一个参数。可以理解其为 `type` 和 `data` 效率上的折衷。

##树

```
data Tree a = Leaf a
            | Node a (Tree a) (Tree a)

data Tree a = Leaf
            | Node a (Tree a) (Tree a)

data Tree a = Node a [Tree a]

data Tree a = Node [a] [Tree a]

-- Haskell Data.Tree 的定义

data Tree a = Node {
              rootLabel :: a,
              subForest :: Forest a
              }
type Forest a = [Tree a]
```

##一般化的代数数据类型

```
data Exp = ValInt  Int
         | ValBool Bool
         | Add (Exp a) (Exp a)
         | Equ (Exp a) (Exp a)
         deriving (Show, Eq)
```

以上为一个 DSL，包括数字和布尔类型，还包括一组运算和逻辑判断。为了保证表达式的正确性，还令 `Exp` 类型参数化，增加了参数类型 `a`，称为*虚幻类型*（phantom type）。

这样以来，在 `eval` 时，需要对 `Exp` 的 `Equl` 增加一个匹配，需要用 `Either` 区别加法和判断相等运算：

```
eval :: Exp -> Either Int Bool
eval (ValInt  a) = Left  a
eval (ValBool b) = Right b
eval (Add e1 e2) = case eval e1 of
                   Left a -> case eval e2 of
                             Left b -> Left (a + b)
eval (Equ e1 e2) = case eval e1 of
                   Left a -> case eval e2 of
                             Left b -> Right (a == b)
```

通过 GADT，可以重写之前的类型定义：

```
data Exp a where
    ValInt  :: Int  -> Exp Int
    ValBool :: Bool -> Exp Bool
    Add     :: Exp Int -> Exp Int -> Exp Int
    Equ     :: Exp Int -> Exp Int -> Exp Bool
```

有了明确的类型限定以后，也可以重写 `eval` 函数：

```
eval :: Exp a -> a
eval (ValInt  i) = i
eval (ValBool b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Equ e1 e2) = eval e1 == eval e2
```

如此一来，不合法的表达式就能通过类型系统检查出来，不会导致未定义行为。GADT 定义数据类型和常规方法的区别在于：GADT 里，需要明确指定每个构造器的类型。但也享受了更大的自由，因为不会由编译器自动推导而受限。

##类型的 kind

kind 是对类型的进一步抽象。

看不懂了。
















