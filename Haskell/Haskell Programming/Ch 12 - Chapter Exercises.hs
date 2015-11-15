module Test where

import Data.List

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

countVowel str = sum $ map isVowel str 
                 where isVowel = \x -> if elem x "aeiou" then 1 else 0

mkWord :: String -> Maybe Word'
mkWord str | numVowel > length str - numVowel = Nothing
           | otherwise = Just (Word' str)
           where numVowel = countVowel str

-- It's only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ $ integerToNat $ n - 1

-- Small library for Maybe

isJust :: (Eq a) => Maybe a -> Bool
isJust x | x /= Nothing = True
         | otherwise    = False

isNothing :: (Eq a) => Maybe a -> Bool
isNothing x = not $ isJust x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing  = b
maybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe b Nothing  = b
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = (maybeToList x) ++ (catMaybes xs)

flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe x | any (==Nothing) x = Nothing
            | otherwise = Just $ catMaybes x

-- Small library for Either

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight x = not $ isLeft x

getLeft :: Either a b -> a
getLeft (Left x) = x

getRight :: Either a b -> b
getRight (Right x) = x

lefts' :: [Either a b] -> [a]
lefts' x = foldr ((:) . getLeft) [] $ filter isLeft x

rights' :: [Either a b] -> [b]
rights' x = foldr ((:) . getRight) [] $ filter isRight x

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = (lefts' x,  rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  x) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  x) = f x
either' _ g (Right x) = g x

-- fold

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
            where go :: Num a => a -> [a] -> a
                  go n [] = n
                  go n (x:xs) = (go (n+x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
                where go :: Num a => a -> [a] -> a
                      go n [] = n
                      go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

-- Write your own iterate and unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

getJust :: Maybe a -> a
getJust (Just x) = x

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                Nothing    -> []
                Just (a,b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- Finally something other than a list

data BinaryTree a = Leaf 
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a  = Node (insert' b left) a right
                              | b > a  = Node left a (insert' b right)