-- IDENTIFICAÇÃO
matricula = "536334"

-- Nome
nome = "Mateus Lima Rodrigues" 

-- ATIVIDADE 3

parcialStrip :: [Char] -> [Char]
parcialStrip (x:xs) = if x == ' ' then strip xs else x:xs
parcialStrip xs = xs

strip :: [Char] -> [Char]
strip xs = reverse (parcialStrip (reverse (parcialStrip xs)))

popWord :: [Char] -> ([Char], [Char])
popWord xs = (takeWhile (/= ' ') xs, dropWhile (/= ' ') xs)
--popWord xs = ("", "") -- implemente aqui

splitStr :: [Char] -> [[Char]]
splitStr xs = if xs == "" then [] else (fst (popWord xs)) : splitStr (strip (snd (popWord xs)))