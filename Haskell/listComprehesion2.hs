--------------------------------------------------------------------
--Função que gera o cartesiano AxA em um conjunto finito de inteiros A

cartesiano :: Int -> [Int] -> [(Int,Int)]
cartesiano _ [] = []
cartesiano a (b:c) = (a,b) : cartesiano a c
 
funcao1 :: [Int] -> [Int] -> [(Int,Int)]
funcao1 [] _ = []
funcao1 _ [] = []
funcao1 (a:b) x = cartesiano a x ++ funcao1 b x

--com list comprehension
funcao2 :: [Int] -> [(Int,Int)]
funcao2 x = [(a,b)| a <- x, b <- x]

--------------------------------------------------------------------
--Função que gera o cartesiano AxB em um conjunto finito de inteiros A e B

func3 :: [t] -> [a] -> [(t, a)]
func3 x y = [(a,b) | a <- x, b <- y]

--------------------------------------------------------------------
--Função que gera a relação maior em um conjunto A finito de inteiros

geraMaior :: [(Int,Int)] -> [(Int,Int)]
geraMaior l = [(a,b) | (a,b) <- l, a > b]

--------------------------------------------------------------------
--Função que gera a relação identidade entre dois conjuntos A e B

--Sem list Comprehension
comparaIdentidade :: Int -> [Int] -> [(Int, Int)]
comparaIdentidade _ [] = []
comparaIdentidade a (b:c)
 |a == b = [(a,b)]
 |otherwise = comparaIdentidade a c

geraIdentidade1 :: [Int] -> [Int] -> [(Int,Int)]
geraIdentidade1 _ [] = []
geraIdentidade1 [] _ = []
geraIdentidade1 (a:b) c = comparaIdentidade a c ++ geraIdentidade1 b c


--Com list Comprehension
geraIdentidade2 :: [Int] -> [Int] -> [(Int,Int)]
geraIdentidade2 a b = [(x,y)| x <- a, y <- b, x == y]

--------------------------------------------------------------------
--Função que receba o parâmetro [(x,[y])] do tipo [(Int,[Int])] e retorna [(x,z)] do tipo [(Int,Int)]
--tal que z é a quantidade de ocorrência de x em [y]

--Sem list Comprehension
retornaOcorrencia1 :: [(Int,[Int])] -> [(Int,Int)]
retornaOcorrencia1 [] = []
retornaOcorrencia1 ((a,b):c) = (a, ocorrencia (a,b)) : retornaOcorrencia1 c


--Com list Comprehension
ocorrencia ::  (Int,[Int]) -> Int
ocorrencia (a,[]) = 0
ocorrencia (a,(b:c))
 |a == b = 1 + ocorrencia (a,c)
 |otherwise = ocorrencia (a,c)


retornaOcorrencia2 :: [(Int,[Int])] -> [(Int,Int)]
retornaOcorrencia2 l = [(a, ocorrencia (a,b)) | (a,b) <- l]

--------------------------------------------------------------------
--Função que receba duas lista [int] e retorne ([Int],[Int]) em que a primeira
--contenha os números pares ordenados e a segunda os números ímpares ordenados

geraPar :: [Int] -> [Int]
geraPar l = [x | x <- l, (mod) x 2 == 0]

geraImpar :: [Int] -> [Int]
geraImpar l = [x | x <- l, (mod) x 2 == 1]


--Função ordena
minimo :: [Int] -> Int
minimo [x] = x
minimo (a:b)
 |a <= minimo b = a
 |otherwise = minimo b

remove :: Int -> [Int] -> [Int]
remove x l = [y|y <- l, y /= x]

ordena :: [Int] -> [Int]
ordena [] = []
ordena x = minimo x : ordena (remove (minimo x) x) 

--Função Principal
geraParImparOrd :: [Int] -> ([Int],[Int])
geraParImparOrd l = (ordena (geraPar l),ordena (geraImpar l))

--------------------------------------------------------------------
--Função que receba um x do tipo char, um s do tipo string e retorne (x,[z]) em que [z] do tipo [Int] 
--contendo as posições em que x ocorre em s

findX c s = (c,[b| (a,b) <- (zip s [0,1..]) , a == c])

--------------------------------------------------------------------
--Função que receba [(Int,Char,String)] e retorne ([(Int,Char,String)],[(Int,Char,String)])
--em que no primeiro caso o char está presente na posição da string determinado pelo Int
--e no segundo caso o char não está na posição

elemento _ [] = False
elemento c (a:b) 
 |c == a = True
 |otherwise = elemento c b
 
findY c s = [b| (a,b) <- (zip s [0,1..]) , a == c]

separaLista l = ([x|x@(a,b,c) <- l, (elemento a (findY b c))] , [y|y@(a,b,c) <- l, elemento a (findY b c) == False ])
