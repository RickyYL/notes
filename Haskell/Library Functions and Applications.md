#Library Functions and Applications

预加载库 Prelude 相当于初始环境，定义了许多类型。

##基本函数

* 恒值函数 `id :: a -> a`，返回给定的值。
* 常值函数 `const :: a -> b -> a`，给定两个元素，返回第一个。
* 参数反置函数 `flip :: (a -> b -> c) -> b -> a -> c`
* 错误函数 `error :: String -> a`
* 未定义函数 `undefined :: a`
* `min` & `max`

##基于列表的函数

* `null :: [a] -> Bool`：判断列表是否为空
* `length :: [a] -> Int`：返回列表长度，因为是 Int，所以精度有限
* `!! :: [a] -> Int -> a`：取索引
* `reverse`：将列表倒置
* `head` & `last`：返回首尾元素
* `init` & `tail`：返回去掉首尾的列表
* `map`：将一个函数应用到列表中每一个元素
* `filter`：根据条件过滤掉不满足条件的元素
* `take` & `drop`：从列表连续选取或扔掉相应数量元素
* `snap` & `break`：根据条件，从左到右，直到第一个不符合或符合的元素时停止
* `takeWhile` & `dropWhile`
* `splitAt`：在列表任意一个位置分开
* `repeat` & `replicate`：无限重复或重复给定数量个元素
* `any` & `all`：查询一个列表中是否或全是符合条件的元素
* `elem` & `notElem`：判断一个列表中是否存在一个元素
* `iterate`：不断将第一个函数应用在第二个参数上多次
* `until`：迭代生成数据直到满足给定条件为止
* `zip`：将两个列表结合成一个元祖的列表
* `unzip`：拆开`zip`
* `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`
* `concat`：将一个列表中的列表相连
* `concatMap`：作用与 `concat $ map ...` 相同

##字符串函数

* `show` & `read`
* `lines` & `unlines`
* `words` & `unwords`