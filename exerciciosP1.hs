-- 1
ehPrimo :: Int -> Bool
ehPrimo n
    | n <= 1    = False
    | n == 2    = True
    | otherwise = ehPrimoAux n 2
    where
        ehPrimoAux :: Int -> Int -> Bool
        ehPrimoAux n divisor
            | divisor >= n           = True
            | mod n divisor == 0   = False
            | otherwise              = ehPrimoAux n (divisor + 1)

-- 2
ordenaEmTupla :: Int -> Int -> Int -> Int -> (Int,Int,Int,Int)
ordenaEmTupla 0 0 0 0 = (0,0,0,0)
ordenaEmTupla a b c d
    | a > b = ordenaEmTupla b a c d
    | b > c = ordenaEmTupla a c b d
    | c > d = ordenaEmTupla a b d c
    | otherwise = (a, b, c, d)

-- 3
quantosDias :: Int -> Int
quantosDias x
    | x == 0 = 366
    | x < 0 = 365
    | otherwise = quantosDias (x-4)

-- 4
diasMes :: Int -> Int -> Int
diasMes a m
    | m == 2 && auxMes a == 366 = 29
    | m == 2 && auxMes a == 365 = 28
    | m <= 7 && mod m 2 == 0 = 30
    | m >= 8 && mod m 2 /= 0 = 30 
    | otherwise = 31
    where
        auxMes :: Int -> Int
        auxMes x
            | x == 0 = 366
            | x < 0 = 365
            | otherwise = auxMes (x-4)

-- 5
dia :: Int -> Int -> Int -> Int
dia a m d
    | m == 1 = d
    | otherwise = d + dia a (m-1) (diasMes a (m-1))

-- 6
type Meu_tipo = (Int, Int)

buscaMaior :: [Int] -> Int
buscaMaior [] = -1
buscaMaior (head:tail)
    | head >= buscaMaior tail = head
    | otherwise = buscaMaior tail

buscaMenor :: [Int] -> Int
buscaMenor [] = 100000
buscaMenor (head:tail)
    | head <= buscaMenor tail = head
    | otherwise = buscaMenor tail

maiormenor :: [Int] -> Meu_tipo
maiormenor lista = (buscaMenor lista, buscaMaior lista)

-- 7
removeMenor :: [Int] -> Int -> [Int]
removeMenor [] _  = []
removeMenor (head:tail) x
    | head == x = tail
    | otherwise = head : removeMenor tail x

ordena :: [Int] -> [Int]
ordena [] = []
ordena lista = buscaMenor lista : ordena (removeMenor lista (buscaMenor lista))

-- 8
repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (head:tail) = auxRepete head head ++ repeteElemento tail
    where
        auxRepete :: Int -> Int -> [Int]
        auxRepete _ 0 = []
        auxRepete x y = x : auxRepete x (y-1)


-- 9
serie :: Int -> Int -> Int
serie _ 0 = 0
serie x y
    | mod y 2 /= 0 = div y x + serie x (y-1)
    | otherwise = div x y + serie x (y-1)