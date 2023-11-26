periodo::Int
periodo = 7

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

--Retorna o dia de certa venda mas depende de parâmetro
diaVenda :: Int -> Int -> Int
diaVenda 0 _ = 0
diaVenda d v
 |vendas d == v = d
 |otherwise = diaVenda (d - 1) v
 
--Retorna o dia de certa venda
diaVendaS :: Int -> Int
diaVendaS v = diaVenda periodo v 

--Retorna o total de vendas
totalVenda :: Int -> Int
totalVenda 0 = 0
totalVenda d = vendas d + totalVenda (d - 1)

--Função que retorna quantas vendas supera um valor
tVendaSuperior :: Int -> Int -> Int
tVendaSuperior 0 _ = 0
tVendaSuperior d m
 | vendas d > m = 1 + tVendaSuperior (d - 1) m
 |otherwise = tVendaSuperior (d - 1) m
 
--Função que retorna a soma de todas as vendas do periodo para venda par
somaDiaPar :: Int -> Int
somaDiaPar 0 = 0
somaDiaPar d
 |vendas d `mod` 2 == 0 = vendas d + somaDiaPar (d - 1)
 |otherwise = somaDiaPar (d - 1)
 
--Função que retorna o dia de maior venda
maior :: Int -> Int -> Int
maior a b
 | a > b = a
 |otherwise = b
 
 --Retorna a maior venda
maiorVenda :: Int -> Int -> Int
maiorVenda 0 valor = valor
maiorVenda dia valor = maiorVenda ( dia - 1) ( maior (vendas dia) valor )

--Retorna a maior venda `alternativa`
maiorVendaA :: Int -> Int
maiorVendaA 0 = 0
maiorVendaA dia = maior (vendas dia) (maiorVendaA (dia - 1))