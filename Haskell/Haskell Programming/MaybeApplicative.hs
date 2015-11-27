module MaybeApplicative where

validateLength :: Int -> String -> Maybe String
validateLength maxlen s = if (length s) > maxLen
                          then Nothing
                          else Just s



newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a



data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = case mkName of
               Nothing -> Nothing
               Just n' -> case mkAddress of 
                          Nothing -> Nothing
                          Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a