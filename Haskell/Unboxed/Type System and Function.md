#Type System and Function

Haskell 不仅有普通的类型系统，而且还将有着共同属性的类型归为一种特定的*类型类（typeclass）*。可以通过 `Eq` 判断两个值是否相等。

##Haskell 的类型与数据

Haskell 是强类型语言，且约定每个类型及类型数据成员首字母大写。

###常用数据类型

* Bool
* Char
* Int：有符号整数
* Word：无符号整数，需要导入 `Data.Word` 库
* Integer：任意精度整数
* Float, Double
* Rational：任意精度有理数，e.g. `4.1332::Rational`
* String：字符串，通过 `[char]` 定义
* 元组类型：(Type1, Type2, ..., TypeN)，可以通过 `fst` 和 `snd` 获取二元组的两个元素
* 列表类型：所有元素都为同类型，如此定义
** 空列表 `[]`
** 一个元素和另一个列表的结合 `x:xs`

###函数类型

函数可以理解为从参数到结果的一个映射，即 `f:t1->t2`。

* Curried Function，即偏函数。e.g. `let add' x y = x + y :: Int`，`add' 4` 即为一个 Curried Function。
* 多态函数。通过类型变量 `a,b,c,...` 表示不确定类型。

###类型的别名

通过 `type` 指定类型别名，e.g. `type RGB = (Int, Int, Int)`。

别名的定义只是进行名称替换，而不是定义新类型。

###类型系统的重要性

* 错误检查
* 程序抽象
* 文档作用

##数据类型类

* 相等类型类：`Eq`
* 有序类型类：`Ord`，一定是相等类型类
* 枚举类型类：`Enum`，e.g. `[1..2]`，`succ 1`，`pred 'M'`
* 有界类型类：`Bounded`
* 数字类型类：`Num`
* 可显示类型类：`Show`

对于数字类型类，有一组常用函数：
* `fromInteger`, `toInteger`
* `fromRational`, `toRational`
* `fromIntegral`
* `truncate`
* `floor`, `ceiling`, `round`

对于每一个合法的函数，Haskell 能够利用*类型推断系统（type inference system）*给出合理的推断。

##函数

需要明确一个概念，在 Haskell 中，数据与函数是统一的，没有区别。函数不过是需要参数的数据而已。对于一个特定输入，总能返回确定结果的函数，称之为*纯函数*。

函数的定义模式如下：
```
函数名 :: 参数1类型 -> 参数2类型 -> ... -> 结果类型
函数名 参数1 参数2 ... = 函数体
```

### lambda 表达式
lambda 表达式定义如下：
```
函数名 :: 参数1类型 -> 参数2类型 -> ... -> 结果类型
函数名 \参数1 -> \参数2 -> ... -> 函数体
```

同时，Haskell 还引入了 lambda 演算的三条规则：
* α-conversion，不引发名字冲突的情况下，替换参数名
* β-reduction，将参数带入函数体
* η-reduction，消除冗余 lambda 表达式

###单一同态限定 Monomorphism Restriction

看不懂，以后再说

### lambda 表达式的应用

主要有两个应用：
* 对于柯里化函数，不给定前一个参数的请款下给定后一个
* 匿名函数

###参数绑定

例如海伦公式
$$ S = \sqrt{p(p-a)(p-b)(p-c)}, p = \dfrac{a+b+c}{2}. $$

可以通过 `let ... in ...` 在函数定义中做替换：
```
s :: Double -> Double -> Double -> Double
s a b c = let p = (a+b+c)/2 in sqrt (p * (p-a) * (p-b) * (p-c))
```

也可以通过 `where` 进行定义：
```
s' :: Double -> Double -> Double -> Double
s' a b c = sqrt (p * (p-a) * (p-b) * (p-c))
           where p = (a+b+c)/2
```

##表达式

* 条件表达式 `if ... then ... else ...`
* 守卫表达式 `|`
```
abs :: Num a => a -> a
abs n | n > 0 = n
      | otherwise = -n
```
* 情况分析表达式 `case ... of ...`
```
month :: Int -> Int
month n case n of
	1 -> 31
	2 -> 28
	...
	12 -> 31
	_ -> error "Invalid month"
```
* 模式匹配，将不同情况由上到下依次列出
```
month :: Int -> Int
month 1 = 31
month 2 = 28
...
month 12 = 31
month _ = error "Invalid month"
```

##运算符

运算符有三种属性：
* 优先级
* 结合性
* 位置

很多情况下，要对负数加括号。

`($)` 可以改变结合性，例如 `f (g (h x))` 可以写为 `f $ g $ h x`。

```
($) :: (a -> b) -> a -> b
f $ x = f x
```