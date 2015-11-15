#I/O

* Haskell 中的函数不存在副作用。
* Haskell 能够将程序的非纯粹部分从纯粹部分中分离出来，处理有副作用的操作。

##Hello, World!

```
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "Hello, world!"
putStrLn "Hello, world!" :: IO ()
```

* 可以这样理解 `putStrLn` 的类型：取一个字符串作为参数，返回一个 `IO` 操作，而 `IO` 操作返回类型为 `()`，即空元组，也称为**单元**（unit）。

##组合 IO 操作

```
main = do
	foo  <- utStrLn "Hello, what's your name?"
	name <- getLine
	putStrLn $ "Hey " ++ name ++ ", you rock!"
```

* 这里用 `do` 组合起一系列 IO 操作，成为一个 IO 操作。
* `main` 类型为 `IO something`。
* `foo` 被绑定为 `()`，因而没什么意义。
* `do` 代码块不允许为最后一个操作绑定名字，可以理解为，其代码块自动将最后一个操作的值取出。作为他自己产生的返回值。

```
ghci> :t getLine
getLine :: IO String
```

* `getLine` 是一个产生字符串的 IO 操作。
* 可以看把 IO 操作看成一个活动容器，可以在外面做事，也可以带东西回来。一旦它取得了数据，要用 `<-` 才能取得其内容。
* 只有 IO 操作的上下文中才能读取 IO 操作的内容。

###`return`

* `return` 能够基于一个纯的值构造 IO 操作。即取一个值作为参数，包装到容器中。
* 在处理 IO 操作的 `do` 代码块中，`return` 通常是为了创建一个什么都不做的 IO 操作，或者希望返回一个不同的结果，而不仅仅是最后一个 IO 操作的结果。

##常用 IO 函数

* `putStr`：打印字符串后不换行。
* `putChar`：打印字符。
* `print`：等价于 `putStrLn . show`。
* `when`：定义于 `Control.Monad`，取一个布尔值和 IO 操作，布尔值为真则返回 IO 操作，否则返回 `reutrn ()`。
* `mapM`, `mapM_`
* `forever`
* `forM`
