-- ATIVIDADE 
atividade = 5

-- IDENTIFICAÇÃO
matricula = "536334" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Mateus Lima Rodrigues" -- coloque seu nome aqui entre aspas

import Data.List (sortOn)

freq :: [Char] -> [(Char, Int)]
freq [] = []
freq (x:xs) = (x, count x (x:xs)) : freq (filter (/= x) xs)
  where count c [] = 0
        count c (y:ys) | c == y = 1 + count c ys
                       | otherwise = count c ys

freqSort :: [(Char, Int)] -> [(Char, Int)]
freqSort = sortOn snd