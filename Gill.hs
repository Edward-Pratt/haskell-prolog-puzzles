
number :: [Int] -> Int
number [] = 0
number (x:xs) = x * 10 ^ length xs + number xs

generator1:: [([Int],[Int], [Int])]
generator1 
    = [([x1,x2,x3],[x4,x5,x6],[x7,x8,x9])
    | x1 <- [0..9]
    , x2 <- [0..9], notElem x2 [x1]
    , x3 <- [0..9], notElem x3 [x1,x2]
    , x4 <- [0..9], notElem x4 [x1,x2,x3]
    , x5 <- [0..9], notElem x5 [x1,x2,x3,x4]
    , x6 <- [0..9], notElem x6 [x1,x2,x3,x4,x5]
    , x7 <- [0..9], notElem x7 [x1,x2,x3,x4,x5,x6]
    , x8 <- [0..9], notElem x8 [x1,x2,x3,x4,x5,x6,x7]
    , x9 <- [0..9], notElem x9 [x1,x2,x3,x4,x5,x6,x7,x8]
    ]

selector1 :: ([Int],[Int],[Int]) -> Bool
selector1 (s1,s2,s3) = sumListOdd s1 && sumListOdd s2 && sumListOdd s3 && isPrime (number s1) && isPrime (number s2) && isPrime (number s3) && equallySpaced [number s1, number s2, number s3]
       

sumListOdd :: [Int] -> Bool
sumListOdd [x1,x2,x3] = (x1 + x2 + x3) `mod` 2 == 1


-- Used From Lectures
isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = not (factorisable n 2)
        where
            factorisable n f
                | f * f > n = False
                | n `mod` f == 0 = True
                | otherwise = factorisable n (f + 1)

equallySpaced :: [Int] -> Bool
equallySpaced [x1,x2,x3] = x2 - x1 == x3 - x2


main::IO()
main = do
    print(filter selector1 generator1)