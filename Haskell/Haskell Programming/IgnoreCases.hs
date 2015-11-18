module IgnoreCases where

--------------------------------------------------
-- Maybe type
--------------------------------------------------

-- someFunc Nothing = Nothing
-- someFunc (Just x) = Just $ someOtherFunc x

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just a) = Just $ show s
showIfJust Nothing = Nothing

-- someFunc x = fmap someOtherFunc x

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

-- eta contract

incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

-- more generic for all Functors

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

--------------------------------------------------
-- Either type
--------------------------------------------------

incIfEither :: Num a => Either e a -> Either e a
incIfEither = fmap (+1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show
