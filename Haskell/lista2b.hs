--Usando uma compreensão de lista, forneça uma expressão que calcule a soma 1^2 +
--2^2 + ... 100^2 dos primeiros cem quadrados inteiros.

sqr100 = sum[(a*a) | a <- [1..100]]

-- -----------------------------------------
-- função que replica um 'a' 'x' vezes em uma lista
replica :: a -> Int -> [a]
replica a x = [a| _ <- [1..x]]

-- -----------------------------------------

-- Função que retorna a lista de todos os triplos pitagóricos cujos componentes são no máximo um
--dado limite.
pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x,y,z)|x <- [1..a],y <- [1..a],z <- [1..a], x^2 + y^2 == z^2 ]

-- -----------------------------------------

--Um inteiro positivo é perfeito se for igual à soma de seus fatores, excluindo
--o próprio número. Usando uma compreensão de lista e os fatores de função, defina um
--function perfects :: Int -> [Int] que retorna a lista de todos os números perfeitos acima
--a um determinado limite. Por exemplo: > aperfeiçoa 500
--[6,28,496]

factors :: Int -> [Int]
factors a = [x |x <- [1..a-1], (mod) a x == 0]

perfects :: Int -> [Int]
perfects a = [x | x <- [1..a], (sum (factors x)) == x ]

-- -----------------------------------------

--Defina a função find usada nas posições de função.

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
 where n = (length xs) - 1
 
--Resultado

--Sem list comprehension
--find _ [] = []
--find x ((a,b):c)
-- |x == a = b : find x c
-- |otherwise = find x c 

--com list comprehension
find x l = [b|(a,b) <- l, a == x]

-- -----------------------------------------
-- Função que faz o produto escalar entre duas listas

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar xl yl = sum[a*b |(a,b) <- (zip xl yl)]

-- -----------------------------------------
infixl  7 &!
(&!) :: Int -> Int -> Int
(&!) x y = product[x |_ <- [1..y]]

-- -----------------------------------------

-- Mostre como a compreensão da lista [f x | x <- xs, p x] pode ser reexpresso
--usando as funções de ordem superior
--mapear e filtrar. Tente entender e aplicar o exemplo [(+7) x | x <- [1..10],
--odd x]
--R: se p x for verdadeiro, será feito alguma operação em x de acordo com a função f, em que x é cada elemento de xs
--ela pode ser reexpressa mapeando a lista xs de acordo com f e filtrando x com p, ou seja, os x que entrarem no filtro,
--é feito o mapeamento em x de acordo com f e jogado na lista
--Ex dado: nesse exemplo o filtro é o odd x ou seja para todo x entre [1..10], os números que forem ímpares são somados em 7
--e jogados na lista

-- -----------------------------------------

--Defina uma função dec2int :: [Int] -> Int que converte um número decimal
--em um número inteiro. por exemplo:
-- > dec2int [2,3,4,5] 2345

decimal :: [(Int,Int)] -> Int
decimal [] = 0
decimal ((a,b):c) = a * 10^b + decimal c

dec2Int :: [Int] -> Int
dec2Int x = decimal (zip x [b|b <- [length x-1, length x -2 ..0]])