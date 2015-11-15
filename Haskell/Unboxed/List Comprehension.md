#List Comprehension

* 生成列表的时候可以使用多个生成器，但是写在后边的生成器会被先遍历。

```
[(x,y) | x <- [1..4], y<-[1..4]]
```

* 用列表生成器也可以定义 `map` 函数：

```
map f xs = [f x | x <-xs]
```

* 同时也可以加以谓词限定，定义 `filter` 函数：

```
filter f xs = [x | x <- xs, f x]
```

* 同时也可以用 `_` 通配符，定义 `length` 函数：

```
length xs = sum[1 | _ <- xs]
```

* 生成乘法口诀表：

```
[show x ++ "*" ++ show y ++ "=" ++ show (x*y) | y <- [1..0], x <- [1..y]]
```

* 生成 `pi`：

```
series n = [1/(2*(fromIntegral k)+1) * (-1)^k | k <-[0..n]]
pi = 4 * (sum $ series n)
```

##排列组合问题

* 求排列（permutation）
```
insert a :: a -> [a] - [[a]]
insert n [] = [[n]]
insert n (n':ns) = (n:n':ns):[n':ns' | ns'<-insert n ns]

permutation [] = [[]]
permutation (x:xs) = concat [insert x permuxs | permuxs <- permutation xs]
```















