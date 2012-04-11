explode :: (Integral a) => a -> [a]
explode 0 = []
explode x =  [(mod x 10)] ++ (explode (div x 10) )

collapse :: (Integral a) => [a] -> a
collapse [] = 0
collapse (x:xs) =  (10 * (collapse xs )) +  x


add :: (Integral a) => [a] -> [a] -> [a]
add a b = zipWith (+) a b

shiftr :: (Integral a) => [a] -> [a]
shiftr x = 0:x
-- extend :: [a] -> a -> [a]
extend x n = x ++ (take n (repeat 0))
