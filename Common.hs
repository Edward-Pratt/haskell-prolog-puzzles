


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
selector2 (a1, a2, a3, a4, a5, a6, a7, a8)
    = mutualDigits [square a1, square a2, square a3] --Shares A
    && mutualDigits [square a1, square a4] --Shares L
    && mutualDigits [square a1, square a5] -- Shares N
    && mutualDigits [square a2, square a4, square a5, square a6, square a8] -- Shares C
    && mutualDigits [square a2, square a6, square a8] --Shares R
    && mutualDigits [square a2, square a4, square a6] -- Shares Y
    && mutualDigits [square a3, square a7] -- Shares E and Shares S
    && mutualDigits [square a5, square a6, square a8] -- Shares I
    && mutualDigits [square a5, square a6] -- Shares K
    && mutualDigits [square a7, square a8] -- Shares T and V







mutualDigits :: [Int] -> Bool
mutualDigits [] = False
mutualDigits [x] = not (null (digits x))
mutualDigits (x:xs) = hasMutualwithOthers (digits x)(map digits xs)

hasMutualwithOthers :: [Int] -> [[Int]] -> Bool
hasMutualwithOthers _ [] = True
hasMutualwithOthers [] _ = False
hasMutualwithOthers ds (ys:yss)
    | not (null (common ds ys)) = hasMutualwithOthers (common ds ys) yss
    | otherwise                 = False

common:: [Int] -> [Int] -> [Int]
common [] _ = []
common (x:xs) ys
    | x `elem` ys = x : common xs ys
    | otherwise   = common xs ys



x_warmup2 :: Int
x_warmup2
    = n5
        where
        n1 = x_unit ( digits 0 == [0]) 0
        n2 = x_unit ( digits 5 == [5]) n1
        n3 = x_unit ( digits 47 == [4 ,7]) n2
        n4 = x_unit ( digits 620 == [6 ,2 ,0]) n3
        n5 = x_unit ( digits 2745 == [2 ,7 ,4 ,5]) n4


x_generator2 :: Int
x_generator2 = n10
    where
        n1 = x_unit (elem (14,17,27,13,24,29,23,19) generator2) 0
        n2 = x_unit (elem (16,24,25,13,19,14,29,23) generator2) n1
        n3 = x_unit (elem (17,27,28,13,24,23,25,14) generator2) n2
        n4 = x_unit (elem (24,14,17,13,23,25,16,27) generator2) n3
        n5 = x_unit (elem (29,28,14,13,23,18,19,16) generator2) n4
        n6 = x_unit (notElem (14,24,13,13,27,25,17,29) generator2) n5
        n7 = x_unit (notElem (13,31,25,13,17,14,23,27) generator2) n6
        n8 = x_unit (notElem (25,17,19,13,25,24,28,29) generator2) n7
        n9 = x_unit (notElem (16,19,14,13,18,18,19,28) generator2) n8
        n10 = x_unit (notElem (13,14,18,13,25,29,16,25) generator2) n9

x_selector2 :: Int
x_selector2
    = n10
    where
    n1 = x_unit ( selector2 (19 ,31 ,29 ,16 ,25 ,23 ,28 ,27) ) 0
    n2 = x_unit ( not ( selector2 (14 ,16 ,25 ,13 ,31 ,23 ,24 ,27) ) ) n1
    n3 = x_unit ( not ( selector2 (14 ,24 ,27 ,13 ,23 ,16 ,17 ,31) ) ) n2
    n4 = x_unit ( not ( selector2 (16 ,17 ,23 ,13 ,25 ,14 ,24 ,27) ) ) n3
    n5 = x_unit ( not ( selector2 (16 ,27 ,18 ,13 ,25 ,14 ,19 ,31) ) ) n4
    n6 = x_unit ( not ( selector2 (17 ,14 ,23 ,13 ,16 ,24 ,25 ,19) ) ) n5
    n7 = x_unit ( not ( selector2 (17 ,16 ,14 ,13 ,25 ,19 ,24 ,31) ) ) n6
    n8 = x_unit ( not ( selector2 (19 ,14 ,25 ,13 ,16 ,27 ,31 ,24) ) ) n7
    n9 = x_unit ( not ( selector2 (19 ,25 ,14 ,13 ,16 ,31 ,24 ,23) ) ) n8
    n10 = x_unit ( not ( selector2 (23 ,31 ,14 ,13 ,24 ,27 ,17 ,25) ) ) n9        


x_unit x n
    = if x then n +1 else n

main :: IO()
main=do

    print(x_warmup2)
    print(x_generator2)
    print(x_selector2)