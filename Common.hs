


digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = digits (div n 10) ++ [mod n 10]

generator2 :: [(Int, Int, Int, Int, Int, Int, Int, Int)]
generator2 = [(a1, a2, a3, a4, a5, a6, a7, a8) |
        a1 <- possible
        ,a2 <- possible, notElem a2 [a1]
        ,a3 <- possible, notElem a3 [a1,a2]
        ,a4 <- possible, notElem a4 [a1,a2,a3]
        ,a5 <- possible, notElem a5 [a1,a2,a3,a4]
        ,a6 <- possible, notElem a6 [a1,a2,a3,a4,a5]
        ,a7 <- possible, notElem a7 [a1,a2,a3,a4,a5,a6]
        ,a8 <- possible, notElem a8 [a1,a2,a3,a4,a5,a6,a7]
        ,a4 == minimum [a1,a2,a3,a4,a5,a6,a7,a8]]
    where possible = [x | x <- [10..31], digitsDifferent (digits (square x))]



minumum :: [Int] -> Int
minumum [x]
    = x
minumum (x:xs)
    | x < minumum xs = x
    | otherwise = minumum xs


digitsDifferent:: [Int] -> Bool
digitsDifferent []
    = False
digitsDifferent [x1,x2,x3]
    = x1 /= x2 && x1 /= x3 && x2 /= x3
digitsDifferent _
    = False


square :: Int -> Int
square n = n * n


selector2 :: (Int, Int, Int, Int, Int, Int, Int, Int) -> Bool
selector2 (b1, b2, b3, b4, b5, b6, b7, b8)
    =
        
        (common a1 a2) &&
        (common a1 a3) &&
        (common a1 a4) &&
        (common a1 a5) &&
        not (common a1 a6) &&
        not (common a1 a7) &&
        not (common a1 a8) &&

        (common a2 a3) &&
        (common a2 a4) &&
        (common a2 a5) &&
        (common a2 a6) &&
        (common a2 a8) &&
        not (common a2 a7) &&
        
        (common a3 a7) &&
        not (common a3 a4) &&
        not (common a3 a5) &&
        not (common a3 a6) &&
        not (common a3 a8) &&
        
        (common a4 a5) &&
        (common a4 a6) &&
        (common a4 a8) &&
        not (common a4 a7) &&

        (common a5 a6) &&
        (common a5 a8) &&
        not (common a5 a7) &&

        (common a6 a8) &&
        not (common a6 a7) &&

        (common a7 a8)
        where
            a1 = square b1
            a2 = square b2
            a3 = square b3
            a4 = square b4
            a5 = square b5
            a6 = square b6
            a7 = square b7
            a8 = square b8

   
common:: Int ->Int -> Bool
common x y
    = any(`elem` digits x) (digits y)



main :: IO()
main=do
    print (filter selector2 generator2)

