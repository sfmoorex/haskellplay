memperrin :: Int -> Integer
memperrin = (map perrin [0 ..] !!)
	where 
		perrin 0 = 3 
		perrin 1 = 0
		perrin 2 = 2
		perrin n = memperrin (n - 2) + memperrin (n - 3)
