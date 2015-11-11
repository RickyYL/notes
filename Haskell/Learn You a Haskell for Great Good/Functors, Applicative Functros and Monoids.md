# Functors, Applicative Functors and Monoids

在 Haskell 中，我们不需要把类型之间的关系想象为一个层级结构。相反，我们思考类型的行为是什么，通过类型类将其关联起来。

## Functors

`Functor` 是可以被映射的东西。`fmap` 意为：给我一个【取参数 `a`，返回 `b` 的函数】，以及一个【装有 `a` 的容器】，我会返回一个【装有 `b` 的容器】。`fmap` 在保持上下文不变的条件下，把函数应用到值上。

为了将一个类实现 `Functor` 类型类，其 kind 必须是 `* -> *`，表示取一个具体类型作为类型参数。

### 作为 `Functor` 的 IO 操作

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