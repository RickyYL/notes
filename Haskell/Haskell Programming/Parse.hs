module Parse where

-----------------------------------------------------------
-- The Parser type
-----------------------------------------------------------

type Parser a = [Char] -> Maybe (a, [Char])

-----------------------------------------------------------
-- Basic Parsers
-----------------------------------------------------------

-- | Always succeeds with the result value `v`
-- without consuming any of the input string.

return' :: a -> Parser a
return' v =  \ input -> Just (v, input)

-- | Always fails, regardless of the contents of the input string.

failure :: Parser a
failure = \ input -> Nothing

-- | Fails if the input string is empty,
-- Succeeds with the firsst character as the result value.

item :: Parser Char
item = \ input -> case input of 
                  []     -> Nothing
                  (x:xs) -> Just (x, xs)

-- | Because parsers are functions, they could be applied to a
-- String using normal function application. But here we want 
-- to abstract this pattern.

parse :: (Parser a) -> [Char] -> Maybe (a, [Char])
parse p input = p input
