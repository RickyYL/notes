module Zipper where

-- Zipper for Lists, move forward and backward

data ZipperList a = ZipperList [a] a [a] deriving Show

fromList :: [a] -> ZipperList a
fromList _      = error "empty!"
fromList (x:xs) = ZipperList [] x xs

-- The current position is a.
-- Move backward will push current elem to the left list,
-- and take one elem from right list to the current postion.
-- So does moving forward.

next :: ZipperList a -> ZipperList a
next (ZipperList ys y (x:xs)) = ZipperList (y:ys) x xs
next z = z

prev :: ZipperList a -> ZipperList a
prev (ZipperList (y:ys) x xs) = ZipperList ys y (x:xs)
prev z = z

-- Zipper for Trees, move leftward, rightward or upward

data Tree a = Leaf a
            | Node a (Tree a) (Tree a)

data Accumulate a = Empty
                  | R (Accumulate a) a (Tree a)
                  | L (Accumulate a) a (Tree a)

type ZipperTree a = (Tree a, Accumulate a)

right :: ZipperTree a -> ZipperTree a
right (Node n l r, a) = (r, R a n l)
right a = a

left :: ZipperTree a -> ZipperTree a
left (Node n l r, a) = (l, L a n r)
left a = a

up :: ZipperTree a -> ZipperTree a
up (t, R a n l) = (Node n l t, a)
up (t, L a n r) = (Node n t r, a)
up z@(t, Empty) = z

