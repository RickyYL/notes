module Jammin where
import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Ord, Show)

data JamJars = Jam { fruit :: Fruit
                   , jars  :: Int 
                   } deriving (Eq, Ord, Show)

row1 = Jam Peach 10
row2 = Jam Plum  20
row3 = Jam Apple 30
row4 = Jam Blackberry 40
row5 = Jam Plum  30
row6 = Jam Peach 20
allJam = [row1, row2, row3, row4, row5, row6]

mostRow :: [JamJars] -> JamJars
mostRow x = foldr (\ x y -> if (jars x) > (jars y) then x else y) (head x) (tail x)

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortByFruit :: [JamJars] -> [JamJars]
sortByFruit x = sortBy compareKind x

groupByFruit :: [JamJars] -> [[JamJars]]
groupByFruit x = groupBy (\ x y -> (fruit x) == (fruit y)) x