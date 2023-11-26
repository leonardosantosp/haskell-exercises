--Multiplica dois valores inteiros
mult :: Int -> Int -> Int
mult x  y = y * x

--EX1 a
f1 :: Float -> Float 
f1 x 
 | x >= 0 = (x+4)/(x+2)
 |otherwise =  2/x

--EX1 b
f2 :: Float -> Float -> Float
f2 x y
 |x >= y = x + y
 |otherwise = x - y
	
--EX1 c
f3 :: Float -> Float -> Float -> Float
f3 x y z
 |(x+y) > z = x + y + z
 |(x + y) < z = x - y - z
 |otherwise = 0
	
--EX2
fat :: Int -> Int
fat 0 = 1
fat x = x * fat(x - 1)

--EX3
somaValores :: Int -> Int -> Int
somaValores x y = x + y

multiplicaValores :: Int -> Int -> Int
multiplicaValores 0 _ = 0
multiplicaValores x y = somaValores y (multiplicaValores (x - 1) y)

--EX3 Alternative
multiplica :: Int -> Int -> Int
multiplica 1 y = y
multiplica 0 _ = 0
multiplica x y = somaValores y y + multiplica (x - 2) y

--EX4 
invertNumbers :: Int -> Int -> (Int, Int)
invertNumbers x y = (invertDigits x, invertDigits y)

invertDigits :: Int -> Int
invertDigits n = invertDigitsHelper n 0

invertDigitsHelper :: Int -> Int -> Int
invertDigitsHelper 0 result = result
invertDigitsHelper n result = invertDigitsHelper (n `div` 10) (result * 10 + n `mod` 10)

--EX5
square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower x = square (square x)	

--EX6
iesimo :: Float -> Float 
iesimo 0 = sqrt 6
iesimo i = sqrt (6 + (iesimo (i - 1)))

--EX7
choose :: Int -> Int -> Int
choose 0 _ = 1
choose n m
 | n > 0  && m >= n = m * choose (n - 1) (m - 1)
 | otherwise = 0
 
--EX8
mdc :: Int -> Int -> Int
mdc m 0 = m
mdc m n
 |n > 0 = mdc n (m `mod` n) 
 |otherwise = 0

 
--EX9 
howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples x a b
 |a == b && (a `mod` x) == 0 = 1
 |a == b && (a `mod` x) /= 0 =  0
howManyMultiples x y z
 |y `mod` x == 0 = 1 + howManyMultiples x (y + 1) z
 |otherwise = howManyMultiples x (y + 1) z
 
--EX10
lastDigit :: Int -> Int
lastDigit x
  | x < 10 = x
  |otherwise = lastDigit (x `mod` 10)


--EX11

--Conta o número de dígitos de um número
countDigits :: Int -> Int
countDigits x
 |x >= 10 = 1 + countDigits (x `div` 10)
 |otherwise = 0
 
--Função que retorna 10^x 
count :: Int -> Int
count 0 = 1
count x = 10 * count (x -1)

--Transformando o inteiro em Lista de inteiro
