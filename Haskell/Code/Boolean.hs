--Boolean.hs
import Prelude hiding ((/=), (==), not, and, or, (&&), (||))

(==) :: Bool -> Bool -> Bool
(==) True True = True
(==) False False = False
(==) _ _ = False

not :: Bool -> Bool
not True = False
not _ = True

not' = (== False)

xor, and, or :: Bool -> Bool -> Bool
xor b1 b2 = not $ b1 == b2
and True b1 = b1
and False _ = False 
or False b1 = b1
or True _ = True

--if .. then .. else ..
condition :: Bool -> a -> a -> a
condition True t f = t
condition False t f = f

infix 4 == 
infix 4 /=
infixl 3 &&
infixl 2 ||

(||) = or
(&&) = and
(/=) = xor

--半加器
hA :: Bool -> Bool -> (Bool, Bool)
hA a b = (a /= b, a && b)

--全加器
fA a b c = let (axb, aab)     = hA a b   in
           let (axbxc, axbac) = hA axb c in (axbxc, aab || axbac)