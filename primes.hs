primes  = 2 : sieve [3,5..]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..])

minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

gaps = zipWith (-) (map (primes !!) [1..]) (map (primes !!) [0..])


perrin :: Int -> Integer
perrin = (map perrin' [0..] !!)
	where 
		perrin' 0 = 3
		perrin' 1 = 0
		perrin' 2 = 2
		perrin' n = perrin (n - 2) + perrin (n - 3)
