-- ATIVIDADE 
atividade = 4

-- IDENTIFICAÇÃO
matricula = "536334"

-- Nome
nome = "Mateus Lima Rodrigues"

-- 1
unique :: Eq s => [s] -> [s]
unique [] = []
unique (x:xs) = x : unique (filter (/=x) xs)

-- 2
parcialDelete'min :: (Ord a) => [a] -> [a]
parcialDelete'min [] = []
parcialDelete'min (x:xs) = if x == minimum (x:xs) then xs else x : delete'min xs

delete'min :: (Ord a) => [a] -> [a]
delete'min xs = parcialDelete'min xs -- implemente aqui
