# Define Type

## 数据类型定义

### 枚举类型

```
data Bool = False | True
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq, Ord, Enum)

tommorrow :: Day -> Day
tommorrow Sun = Mon
tommorrow = succ
```

* 通过 `:i` 可以查询类型或数据的相关信息。

### 构造类型

```
type Name   = String
type Author = String 
type ISBN   = String
type Price  = Float

data Book = Book Name Author ISBN Price deriving (Show, Eq)
```

* `data Book` 中的 `Book` 是一个没有参数的**类型构造器**。
* `Book Name Author ISBN Price` 中的 `Book` 是一个有四个参数的**数据构造器**。
* 数据构造器 `Book` 本身也是一个函数，这里类型为：`Book :: Name -> Author -> ISBN -> Price -> Book`。

通过在定义时给出字段名，可以一并构造访问器，例如：

```
data Book = Book { name   :: Name
				 , author :: Author
				 , isbn   :: ISBN
				 , price  :: Price
				 }
```

还有一种情况发生在定义一个 “同时用到构造出的值和构造器中字段的” 函数。这种情况下，可以用 `@` 指定一个名字，例如：

```
increasePrice :: ([Book],[Book]) -> Book -> Float -> ([Book],[Book])

increasePrice (b1, b2) b increment = 
	((b:b1), (Book (name b) (author b) (isbn b) (price b + increment)))
	
increasePrice (b1, b2) (Book nm ath isbn prc) pri =
	((Book nm ath isbn prc):b1, (Book nm ath isbn (prc + pri)):b2)

increasePrice (b1, b2) b@(Book nm ath isbn prc) pri =
	(b:b1, (Book nm ath isbn (prc + pri)):b2)
```

### 参数化类型

参数化类型是**需要类型参数的类型**，例如列表，`Maybe`， `Either`。参数化类型的定义中，类型名称后可以加一个任意类型参数，以供这个类型的数据构造器使用。此外，这个参数可以是任何多态类型，而不是确定类型。例如：

```
data Maybe a = Nothing | Just a
```

### 类型的 kind

根据类型构造器的不同，可以将类型化为不同的 kind，因为*类型构造器是基于类型的运算*。

星号 `*` 是所有类型的类型，它是一个**零元类型构造器**，即不需要任何类型参数，自己本身就是一个完整的类型。

例如：

* `Maybe` 的 kind 是：`* -> *`。
* `Either` 的 kind 是：`* -> * -> *`。
* `(->)` 的 kind 是：`* -> * -> *`。

### 递归类型

递归类型就是定义时用到了正在定义的该类型本身的类。例如我们自己定义一个自然数类，并提供一组运算：

```
data Nat = Zero | Succ Nat deriving (Show, Eq)

natToint :: Nat -> Int
natToint Zero = 0
natToint (Succ n) = 1 + natToint n

intTonat :: Int -> Nat
intTonat 0 = Zero
intTonat n = Succ (intToint (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
```

### 杂合定义类型

#### 枚举构造类型

```
data Shape = Circle { radius :: Float }
           | Rect   { length :: Float, width :: Float }
           deriving (Show, Eq)
```

#### 枚举递归定义

```
data BoolExp = TRUE
			 | FALSE
			 | IF BoolExp BoolExp BoolExp

eval :: BoolExp -> Bool
eval TRUE  = True
eval FALSE = False 
eval (IF cond b1 b2) | eval cond == True  = eval b1
					 | eval cond == False = eval b2
```

#### 参数化递归类型

例如标准库中列表的定义：

```
data [] a = [] | a : [a]
```

我们可以自己实现一个相似的列表类型：

```
data List a = Nil | Cons a (List a) deriving (Show, Eq)
```

同时，还能实现这两个列表类相互的转换：

```
mylistToList Nil = []
mylistToList (Cons x xs) = x : (listToMylist xs)

listToMylist [] = Nil
listToMylist (x:xs) = Cons x (listToMylist xs)
```

### 使用 `newtype` 定义新类型

`newtype` 的限制在于：

* 只能定义单一构造器。
* 该构造器有且仅有一个参数。

## 树

以下为一些不同的树的定义，可以根据需求的不同，选择不同类型的树。

```
data Tree a = Leaf a | Node a (Tree a) (Tree a)
data Tree a = Leaf | Node a (Tree a) (Tree a)
data Tree a = Leaf a | Node (Tree a) (Tree a)
data Tree a = Node a [Tree a]
data Tree a = Node [a] [Tree a]

-- Provided by Haskell Standard Library
import Data.Tree
data Tree a = Node { rootLabel :: a
                   , subForest :: [Tree a]
				   }
```

### 卡特兰数

```
data Tree = Leaf | Node Tree Tree deriving Show

trees :: Int -> [Tree]
trees 0 = [Leaf]
trees n = [Node lt rt | l <- [0..(n-1)], lt <- trees l, rt <- trees (n-1-l)]

brace :: Tree -> String
brace Leaf = ""
brace (Node l r) = "(" ++ brace l ++ ")" ++ brace r
```

### 哈弗曼编码

```
import Data.List (insertBy, sortBy)
import Data.Ord  (comparing)

data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving Show

htree [(_, t)] = t
htree ((w1,t1):(w2,t2):wts) = htree $
							  insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts

serialize (Leaf x) = [(x, "")]
serialize (Branch l r) = [ (x, '0':code) | (x,code) <- serialize l ] ++ 
                         [ (x, '1':code) | (x,code) <- serialize r ]

huffman freq = sortBy (comparing fst) $ 
               serialize $ htree $ sortBy (comparing fst) $
			   [(w, Leaf x) | (x,w) <- freq]
```