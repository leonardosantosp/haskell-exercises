-- ------------------------------------------------------------------
--Função que retorna uma lista com os maiores de cada dupla de Int

--menor :: (Int,Int) -> Int
menor (a,b)
 |a < b = a
 |otherwise = b

--maior :: (Int,Int) -> Int
maior (a,b)
 |a > b = a
 |otherwise = b

casa :: (String,String) -> String
casa (a,b) = a ++ b

listaMaior :: [(Int,Int)] -> [Int]
listaMaior l = [ maior x| x <- l]
	
--com funcao de ordem
listaMaiorG :: ((Int,Int) -> Int) -> [(Int,Int)] -> [Int]
listaMaiorG f l = [ f x| x <- l] 

--com tipo genérico
listaMaiorGG :: ((t,t) -> t) -> [(t,t)] -> [t]
listaMaiorGG f l = [ f x| x <- l] 

--Função operador
operador op (a,b) = op a b