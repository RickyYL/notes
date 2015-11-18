module Optional where
import Data.Monoid

data Booly a = False' | True' deriving (Eq, Show)

instance Monoid (Booly a) where
    mappend False' _ = False'
    mappend _ False' = False'
    mappend True' True' = True'

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty  = Nada
    mappend _ Nada = Nada
    mappend Nada _ = Nada
    mappend (Only x) (Only y) = Only (mappend x y)