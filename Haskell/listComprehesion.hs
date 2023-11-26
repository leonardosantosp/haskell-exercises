--Função que multiplica todos os valores de uma lista por uma constante
multLista :: Int -> [Int] -> [Int]
multLista _ [] = []
multLista x (a : b) = a * x : (multLista x b)

--Função multLista com list comprehesion
multListaComprehesion :: Int -> [Int] -> [Int]
multListaComprehesion k l = [k * x|x <- l]

--Função que multiplica os valores pares de uma lista por uma constante
multListaPar :: Int -> [Int] -> [Int]
multListaPar k l = [k * x| x <- l, (mod) x 2 == 0]

--Função que converte String em lista de inteiros
--funcConverte :: [Char] -> [Int]
--funcConverte s = [(ord x) | x <- s]

--Função que converte String em lista de inteiros, com restrições de ser apenas letras minúsculas
--funcConverte :: [Char] -> [Int]
--funcConverte s = [(ord x) | x <- s, (x >= 'a') && (x <= 'z')]

--Função que filtra lista de inteiros ímpares
funcImpar :: [Int] -> [Int]
funcImpar l = [x| x <- l, (mod) x 2 == 1]

--Função que retorna uma lista com os maiores de cada dupla de int

maior :: Int -> Int -> Int
maior x y
 |x > y = x
 |otherwise = y

listMaior :: [(Int, Int)] -> [Int]
listMaior l = [maior x y |(x,y) <- l]

--Outra alternativa

maior2 :: (Int,Int) -> Int
maior2 (x,y)
 |x > y = x
 |otherwise = y
 
listMaior2 :: [(Int,Int)] -> [Int]
listMaior2 l = [maior2 a| a <- l]

--Outra alternativa
--maior3 :: (Int,Int) -> Int
--maior3 (x,y)
-- |x > y = x
-- |otherwise = y
 
--listMaior3 :: [(Int,Int)] -> [Int]
--listMaior3 l = [maior3 (fst a) (snd a)| a <- l]

--Função que gera uma [Int] de uma [(Bool,Int)] com filtro quando Bool == True

func08 :: [(Bool,Int)] -> [Int]
func08 l = [b | (a,b) <- l, a]

--Outra alternativa
--func09 :: [(Bool,Int)] -> [Int]
--func09 l = [(fst a)| a <- l, (fst x)]

--Função que receba [(Bool,Int)] e retorna [[Int]] tal que se bool for True, a lista da dupla e filtrada retirando os pares,
--caso seja False, a lista não sofre alteração

filtro :: (Bool, [Int]) -> [Int]
filtro (a,b)
 |a = [x | x <- b, (mod ) x 2 == 1]
 |otherwise = b

func10 :: [(Bool,[Int])] -> [[Int]]
func10 l = [filtro a | a <- l]