#Recersive Functions

从某种意义上来说，Haskell 没有内存变量，所有函数都是通过递归和普通函数运算完成的。

一些递归函数思想

```
factional :: Integer -> Integer -> Integer
factional n = if n < 0 then error "n is less than 0"
              else if n == 0 then 1
              else n * factional (n-1)

gcd :: Int -> Int -> Int
gcd x y = if y == 0 then x else gcd y $ x `mod` y

power :: Int -> Int -> Int
power _ 0 = 1
power x n | odd  n = let p = power x ((n-1) `div` 2) in x * p * p
          | even n = let p = power x (n `div` 2) in p * p

product :: [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- reverse version of cons
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

take' n _ | n <= 0 = []
take' _ []         = []
take' n (x:xs)     = take' (n-1) xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

delete' _ [] = []
delete' a (x:xs) = if a == x then delete' a xs
                   else x : delete' a xs
```

##一些递归应用

###麦卡锡的 91 函数
```
mc n | n  > 100 = n - 10
     | otherwise = mc $ mc $ n+11
```

###十进制数字转化为罗马数字

1~10 的罗马数字为：
> I, II, III, IV, V, VI, VII, VIII, IX, X.

```
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
romeAmount = [1000, 900, 500, 400, 90, 50, 40, 10, 9, 5, 4, 1]
pair = zip romeAmount romeNotation

-- return the first number less than n
subtrahend n = head $ dropWhile (\(a,_) -> a > n) pair

-- return the roman number for n
convert 0 = (0, "")
convert n = let (i, st)   = subtrahend n  in
            let (i', st') = convert (n-i) in
            (i', st++st')
```

###十进制转化为二进制
```
binarylize 0 = ""
binarylize n = binarylize (div n 2) ++ (show $ rem n 2)
```

###二分查找
```
-- check if a in xs
search a [] = False
search a xs | m < a = search a behind
            | m > a = search a front
            | otherwise = True
            where (front,m:behind) = splitAt (length xs `div` 2) xs

-- return all a in xs
search' a [] = []
search' a xs | m < a = search' a behind
             | m > a = search' a front
             | otherwise = (takeWhile (==a) (reverse front)) ++ [a] ++ (takeWhile (==a) behind)
             where (front,m:behind) = splitAt (length xs `div` 2) xs
```

##排序算法

###插入排序
```
insert x []     = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y : insert x ys

insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)
```

###冒泡排序
```
swaps []         = []
swaps [x]        = [x]
swaps (x1:x2:xs) | x1 > x2   = x2 : swaps (x1:xs)
                 | otherwise = x1 : swaps (x2:xs)

-- 不动点函数
fix f x = if x == x' then x
          else fix f x'
          where x' = f x

bubbleSort xs = fix swaps xs

bubbleSort' xs | swaps xs == xs = xs
               | otherwise = bubbleSort' $ swap xs

bubbleSort'' [] = []
bubbleSort'' xs = bubbleSort'' initElems ++ [lastElem]
                  where swappedxs = swaps xs
                        initElems = init swappedxs
                        lastElem  = last swappedxs
```

###选择排序
```
delete _ []     = []
delete x (l:ls) | x == l    = ls
                | otherwise = l : delete x ls
selectionSort [] = []
selectionSort xs = min xs : selectionSort $ delete min xs
                   where min = minimum xs
```

###快速排序

最简陋的快排，毫无效率可言
```
quickSort []     = []
quickSort (x:xs) = quickSort min ++ [x] ++ quickSort max
                   where min = filter (<x)  xs
                         max = filter (>=x) xs
```

好了点，但是 pivot 选得不好，效率低下
```
filterSplit _ []     = ([],[])
filterSplit f (x:xs) | f x       = ((x:l),r)
                     | otherwise = (l,(x:r))
                     where (l,r) = filterSplit f xs
quickSort' []  = []
quickSort' [x] = [x]
quickSort' (x:xs) = quickSort' l ++ [x] ++ quickSort' r
                    where (l,r) = filterSplit (<x) xs
```

最标准的快排，用了 mid of thee 标准 pivot 选择

```
filterSplit _ []     = ([],[])
filterSplit f (x:xs) | f x       = ((x:l),r)
                     | otherwise = (l,(x:r))
                     where (l,r) = filterSplit f xs

delete _ []     = []
delete x (l:ls) | x == l    = ls
                | otherwise = l : delete x ls

midOfThree xs = x + y + z - maximum [x, y, z] - minimum [x, y, z]
                where x = head xs
                      y = xs !! (length xs `div` 2)
                      z = last xs

quickSort' []  = []
quickSort' [x] = [x]
quickSort' xs  = quickSort' l ++ [pivot] ++ quickSort' r 
                 where pivot = midOfThree xs
                       (l,r) = filterSplit (<pivot) (delete pivot xs)
```

###归并排序
```
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | x > y     = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)
msort xs = merge (msort x1) (msort x2)
           where (x1, x2) = halve xs
                 halve xs = (take l xs, drop l xs)
                 l = length xs `div` 2
```

##递归与不动点

不动点函数
```
fix :: (a -> a) -> a
fix f = f (fix f)
```

带终止条件的不动点函数
```
fix :: (a -> a) -> a
fix f x | x == f x  = x
        | otherwise = fix f (f x)
```

##无基本条件递归和惰性求值
```
nature = 0 : map (+1) nature
fibs = (0:1:zipWith (+) fibs (tail fibs))
```

##变得懒惰
```
lazyShorter xs ys = shorter xs ys
                    where shorter [] ys = True
                          shorter xs [] = False
                          short (x:xs) (y:ys) = short xs ys
```
















