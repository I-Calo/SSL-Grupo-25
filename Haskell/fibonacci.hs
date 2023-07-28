fibonacci :: Int -> [Int]
fibonacci n
    | n <= 0 = []
    | n == 1 = [0]
    | n == 2 = [0, 1]
    | otherwise = fibonacci' [0, 1] (n - 2)
    where fibonacci' xs cuenta
            | cuenta <= 0 = xs
            | otherwise = fibonacci' (xs ++ [sum $ drop (length xs - 2) xs]) (cuenta - 1)

