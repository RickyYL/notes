	Computer all factors of a given number

> factors :: Integral a => a -> [a]
> factors n = [x | x <- [1..n], mod n x == 0]

	Check is prime

> isPrime :: Integral a => a -> Bool
> isPrime n = factors n == [1,n]

	Returns all primes smaller than a given bound

> primes :: Integral a => a -> [a]
> primes n = [x | x <- [1..n], isPrime x]

	New prime check with high enfficiency

> isPrime' :: Integral a => a -> Bool
> isPrime' 2 = True
> isPrime' n = p > 1 &&
		       (all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n*n <= p) [3,5..])

	Return the next prime bigger than a given seed

> nextPrime :: Integral -> Integer
> nextPrime a | odd a = if isPrime a then a else nextPrime (a+2)
			  | otherwise = nextPrime (a+1)

	The Sieve of Eratosthenes

> sieve = Integral a => [a] -> [a]
> sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
> primes' = sieve [2..]

	Return all prime factors and their coeff of a given number

> primeFactor num = [(last $ takeWhile (\n -> (x^n) `elem` (factors num)) [1..], x) | x <- filter isPrime $ factors num]

