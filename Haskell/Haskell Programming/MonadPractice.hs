import Control.Applicative

bind' :: Monad m => (a -> m b) -> m a -> m b
bind' f a = join $ fmap f a



sequencing :: IO ()
sequencing = do 
    putStrLn "Hello"
    putStrLn "World"

sequencing' :: IO ()
sequencing' =
    putStrLn "Hello" >>
    putStrLn "World"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "Hello" *>
    putStrLn "World"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn



bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing'' = 
    putStrLn "name pls: " >>
    getLine >>=
    \name -> putStrLn("y helo thar: " ++ name)



twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn "age pls: "
    age <- getLine
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age)

twoBinds' :: IO ()
twoBinds' = 
    putStrLn "name pls: " >>
    getLine >>=
    (\name -> putStrLn "age pls: " >>
              getLine >>=
              (\age -> putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age)))



twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]



data Cow = Cow { name   :: String
               , age    :: Int
               , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c = let w = weight c
                    n = name c
                in if n == "Bess" && w > 499
                    then Nothing
                    else Just c

--------------------------------------
-- Trivial Solution
--------------------------------------

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
        case noNegative age' of 
        Nothing -> Nothing
        Just agey ->
            case noNegative weight' of
            Nothing -> Nothing
            Just -> weighty ->
                weightCheck(Cow nammy agey weighty)

--------------------------------------
-- Monad Solution
--------------------------------------

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy   <- noEmpty name'
    agey    <- noNegative age'
    weighty <- noNegative weight'
    weightCheck(Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
        \nammy -> noNegative age' >>=
            \agey -> noNegative weight' >>=
                \weight -> weightCheck(Cow nammy agey weighty)

--------------------------------------
-- Maybe Applicative and Monad
--------------------------------------

f :: Maybe Integer
f = Just 1

g :: Maybe String
g = Just "1"

h :: Maybe Integer
h = Just 42

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do
    a <- f
    b <- g
    c <- h
    return (zed a b c)

zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a, b, c)

doSomething = do
    a <- f
    b <- g
    c <- h
    zed' a b c

--------------------------------------
-- Either Monad
--------------------------------------

type Founded = Int

type Coders = Int

data SoftwareShop = Shop { founded     :: Founded
                         , programmers :: Coders
                         } deriving (Eq, Show)

data FoundedError = NegativeYears  Founded
                  | TooManyYears   Founded
                  | NegativeCoders Coders
                  | TooManyCoders  Coders
                  | TooManyCodersForYears Founded Coders
                  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n | n < 0     = Left $ NegativeYears n
                  | n > 500   = Left $ TooManyYears n
                  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n | n < 0    = Left $ NegativeCoders n
                 | n > 5000 = Left $ TooManyCoders n
                 | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers

--------------------------------------
-- Define Either Monad
--------------------------------------

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    _ <*> (First a) = First a
    (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
    return = Second
    (Second b) >>= f = f b

