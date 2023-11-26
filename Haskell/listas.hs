--Função que reverte lista
reverte :: [Int] -> [Int]
reverte [] = []
reverte (a:b) = reverte (b) ++ [a]

--Retorna apenas os números ímpares de uma lista
getImpar :: [Int] -> [Int]
getImpar [] = []
getImpar (a:x)
 |a `mod` 2 /= 0 = a : getImpar x
 |otherwise = getImpar x
 
 -- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

--Função que retorna uma lista de vendas
listaVendas :: Int -> [Int]
listaVendas 0 = [vendas 0]
listaVendas d = vendas d : listaVendas (d - 1)

--Função que retorna lista de lista com dia e vendas
listaDiaVenda :: Int -> [[Int]]
listaDiaVenda 0 = [[0, vendas 0]]
listaDiaVenda d = [d , vendas d] : listaDiaVenda (d - 1)

--Função que insere um elemento em uma lista ordenada
insereElementoOrdenado :: Int -> [Int] -> [Int]
insereElementoOrdenado y [] = [y]
insereElementoOrdenado x (a:b)
 |x > a = a : insereElementoOrdenado x b
 |x <= a = x : [a] ++ b
 
--Função que insere uma lista em uma lista de lista ordenada
listOrdenada :: [Int] -> [[Int]] -> [[Int]]
listOrdenada y [] = [y]
listOrdenada y (a:x)
 |y < a = y:a:x
 |otherwise = [a] ++ listOrdenada y x  
 