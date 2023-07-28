ordenBurbuja :: Ord a => [a] -> [a]
ordenBurbuja [] = []
ordenBurbuja xs = ordenBurbuja (init orden) ++ [last orden]
    where orden = foldr burbuja [] xs
          burbuja x [] = [x]
          burbuja x (y:ys)
              | x <= y = x:y:ys
              | otherwise = y : burbuja x ys