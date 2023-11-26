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

--Função que retorna uma lista de tupla (dia,venda)
listaTupla :: Int -> [(Int, Int)]
listaTupla 0 = [(0,0)]
listaTupla x = (x, vendas x) : listaTupla (x - 1)

--Função que insere um elemento em uma lista ordenada
f1 :: Int -> [Int] -> [Int]
f1 y [] = [y]
f1 x (a:b)
 |x > a = a : f1 x b
 |x <= a = x : [a] ++ b

--Função que ordena uma lista
f2 :: [Int] -> [Int]
f2 [] = []
f2 (a:x) = f1 a (f2 x)

--Total de vendas com periodo
tVendas :: Int -> Int
tVendas 0 = 0
tVendas x = vendas x + tVendas (x - 1)

--Total de vendas sem periodo
vendasB :: [(Int,Int)]
vendasB = [(7,30),(6,55),(5,91),(4,0),(3,48),(2,72),(1,41),(0,0)]
totalVendasB :: [(Int,Int)] -> Int
totalVendasB [] = 0
totalVendasB ((_,b):d) = b + totalVendasB d

--Percorre o banco e retorna a lista das vendas
funcVendas :: Int -> [Int]
funcVendas 0 = [0]
funcVendas x = vendas x : funcVendas (x - 1)

--Percorre o banco e retorna a lista de dias
funcDias :: Int -> [Int]
funcDias 0  = [0]
funcDias x = x : funcDias (x - 1)

--Recebe duas listas de inteiros e transforma em uma lista de tuplas
func :: [Int] -> [Int] -> [(Int,Int)]
func [] [] = []
func (a:x) (b:y) = (a,b) : (func x y)