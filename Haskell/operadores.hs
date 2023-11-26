--Operadores
infixl 7 &&& 

(&&&) :: Int -> Int -> Int
a &&& b
 | a > b = a
 |otherwise = b

--Soma dois inteiros em uma Tupla
addPar :: (Int,Int) -> Int
addPar (x, y) = x + y

--Inverte uma Tupla
shift :: (((Int,Int),Int) -> (Int, (Int,Int)))
shift ((a,b),c) = (a,(b,c))

--Soma o quadrado d dois Inteiros
sumSquare :: Int -> Int -> Int
sumSquare a b = sq a + sq b
 where sq x = x * x
 
--Retorna o maximo entre 2 valores
max2 :: Int -> Int -> Int
max2 x y
 |x > y = x
 |otherwise = y
 
--Retorna o maior entre 3 inteiros 
max3 :: Int -> Int -> Int -> Int
max3 x y z =  max2 x (max2 y z)

--Conta quantas vezes um número aparece
count :: Int -> Int -> Int -> Int -> Int
count x a b c = xa + xb + xc
 where 
 xa = if x == a then 1 else 0
 xb = if x == b then 1 else 0
 xc = if x == c then 1 else 0
 
--Retorna o máximo entre 3 inteiros e o número de vezes que ele repete
max3Count :: Int -> Int -> Int -> (Int, Int)
max3Count x y z = (max3 x y z, count (max3 x y z) x y z)
