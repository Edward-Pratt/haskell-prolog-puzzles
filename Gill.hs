import Control.Monad.Trans.Select (select)
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

x_warmup1 :: Int
x_warmup1
    = n5
    where
        n1 = x_unit ( number [] == 0) 0
        n2 = x_unit ( number [5] == 5) n1
        n3 = x_unit ( number [4 ,7] == 47) n2
        n4 = x_unit ( number [6 ,2 ,0] == 620) n3
        n5 = x_unit ( number [0 ,2 ,7 ,4] == 274) n4

x_generator1 :: Int
x_generator1
    = n10
    where
        n1 = x_unit ( elem ([2 ,4 ,1] ,[3 ,6 ,5] ,[0 ,7 ,8]) generator1 ) 0
        n2 = x_unit ( elem ([4 ,1 ,6] ,[3 ,0 ,2] ,[5 ,7 ,8]) generator1 ) n1
        n3 = x_unit ( elem ([1 ,0 ,5] ,[3 ,6 ,4] ,[2 ,7 ,8]) generator1 ) n2
        n4 = x_unit ( elem ([1 ,6 ,2] ,[7 ,3 ,5] ,[0 ,4 ,8]) generator1 ) n3
        n5 = x_unit ( elem ([8 ,6 ,4] ,[3 ,5 ,2] ,[0 ,7 ,1]) generator1 ) n4
        n6 = x_unit ( notElem ([0 ,2 ,4] ,[3 ,5 ,2] ,[9 ,7 ,1]) generator1 ) n5
        n7 = x_unit ( notElem ([9 ,8 ,7] ,[0 ,1 ,2] ,[3 ,4 ,8]) generator1 ) n6
        n8 = x_unit ( notElem ([0 ,1 ,2] ,[0 ,3 ,4] ,[4 ,7 ,6]) generator1 ) n7
        n9 = x_unit ( notElem ([9 ,8 ,7] ,[6 ,5 ,4] ,[9 ,2 ,1]) generator1 ) n8
        n10 = x_unit ( notElem ([0 ,8 ,7] ,[0 ,1 ,2] ,[0 ,4 ,6]) generator1 ) n9        

x_selector1 :: Int
x_selector1
    = n10
        where
            n1 = x_unit ( selector1 ([1 ,5 ,7] ,[2 ,8 ,3] ,[4 ,0 ,9]) ) 0
            n2 = x_unit ( selector1 ([4 ,0 ,9] ,[2 ,8 ,3] ,[1 ,5 ,7]) ) n1
            n3 = x_unit ( not ( selector1 ([1 ,0 ,9] ,[2 ,8 ,3] ,[4 ,5 ,7]) ) ) n2
            n4 = x_unit ( not ( selector1 ([5 ,4 ,7] ,[2 ,8 ,3] ,[0 ,1 ,9]) ) ) n3
            n5 = x_unit ( not ( selector1 ([8 ,6 ,4] ,[3 ,5 ,2] ,[0 ,7 ,1]) ) ) n4
            n6 = x_unit ( not ( selector1 ([1 ,5 ,7] ,[0 ,2 ,9] ,[8 ,6 ,3]) ) ) n5
            n7 = x_unit ( not ( selector1 ([0 ,8 ,9] ,[2 ,6 ,3] ,[5 ,7 ,1]) ) ) n6
            n8 = x_unit ( not ( selector1 ([7 ,5 ,1] ,[6 ,8 ,3] ,[0 ,2 ,9]) ) ) n7
            n9 = x_unit ( not ( selector1 ([1 ,9 ,7] ,[5 ,2 ,0] ,[8 ,4 ,3]) ) ) n8
            n10 = x_unit ( not ( selector1 ([0 ,8 ,9] ,[4 ,1 ,2] ,[7 ,3 ,5]) ) ) n9


x_unit x n
    = if x then n +1 else n

main::IO()
main = do
    print x_warmup1
    print x_generator1
    print x_selector1
    print(filter selector1 generator1)