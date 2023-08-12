-- IDENTIFICAÇÃO : Matricula = "536334"
-- NOME : Nome = "Mateus Lima Rodrigues" 

-- ATIVIDADE 2

-- Esta atividade visa construir uma 
-- função que determine os n primeiros números primos

-- Construa as funções a seguir:

-- Determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [y|y <- [2..x], mod x y == 0]

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = if(length(divisores x) == 1) then True else False

-- Cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = take n [x|x <- [2..], eprimo x]


