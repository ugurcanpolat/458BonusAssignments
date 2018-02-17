dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m' d = (d + t1 + k + t2 + t3 + 5 * j) `mod` 7
  where 
    j :: Integer
    j = y `div` 100

    k :: Integer
    k = y `mod` 100

    t1 :: Integer
    t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)

    t2 :: Integer
    t2 = k `div` 4

    t3 :: Integer
    t3 = j `div` 4
