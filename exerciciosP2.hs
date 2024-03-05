-- 1
listaDivisor :: Int -> Int -> [Int]
listaDivisor x div
    | x == div = []
    | mod x div == 0 = div : listaDivisor x (div+1)
    | otherwise = listaDivisor x (div+1)

somaDivisores :: [Int] -> Int
somaDivisores [] = 0
somaDivisores (head:tail) = head + somaDivisores tail

ehPerfeito :: Int -> Bool
ehPerfeito 0 = False
ehPerfeito x
    | somaDivisores (listaDivisor x 1) == x = True
    | otherwise = False

-- 2 
somaChar :: String -> Char -> Int
somaChar [] _ = 0
somaChar (head:tail) c
    | head == c = 1 + somaChar tail c
    | otherwise = somaChar tail c

contaString :: String -> [(Char, Int)]
contaString [] = []
contaString (head:tail) = (head, somaChar (head:tail) head) : contaString tail

-- 3
inverteString :: String -> String
inverteString [] = []
inverteString (head:tail) = inverteString tail ++ [head]

-- 4 - use map on terminal hugs
squares :: Int -> Int
squares x = x*x

-- 5
cartesiano :: [Int] -> [Int] -> [(Int, Int)]
cartesiano lista1 lista2 = [(x, y) | x <- lista1, y <- lista2] ++ [(y, x) | y <- lista2, x <- lista1]

-- 6 - use filter
positives :: Int -> Bool
positives x
    | x > 0 = True
    | otherwise = False

-- 7 - use foldr1
sumDouble :: Int -> Int -> Int
sumDouble x y = 2*x + 2*y

-- 8 - use foldr1
concatena :: String -> String -> String
concatena str1 str2 = str1 ++ str2