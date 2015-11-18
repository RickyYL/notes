module ReplaceExperiment where

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP :: b -> Char
replaceWithP = const 'p'

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- f ~ []
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap.fmap) replaceWithP

-- f ~ [], f1 ~ Maybe
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

triceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
triceLifted = (fmap.fmap.fmap) replaceWithP

-- f ~ [], f1 ~ Maybe, f2 ~ []
triceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
triceLifted' = triceLifted

main :: IO ()
main = do
    putStr "replaceWithP'   lms:    "
    print (replaceWithP' lms)
    putStr "liftedReplace   lms:    "
    print (liftedReplace lms)
    putStr "liftedReplace'  lms:    "
    print (liftedReplace' lms) 
    putStr "twiceLifted     lms:    "
    print (twiceLifted lms)
    putStr "twiceLifted'    lms:    "
    print (twiceLifted lms)
    putStr "triceLifted     lms:    "
    print (triceLifted lms)
    putStr "triceLifted'    lms:    "
    print (triceLifted' lms)