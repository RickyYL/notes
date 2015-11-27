data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    _ <*> (First a) = First a
    (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
    (Second b) >>= f = f b