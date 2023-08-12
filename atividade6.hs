-- ATIVIDADE 
atividade = 6

-- IDENTIFICAÇÃO
matricula = "536334"

-- Nome
nome = "Mateus Lima Rodrigues"

data List a = Empty | Node a (List a)

listMap :: (t -> a) -> List t -> List a
listMap _ Empty = Empty -- implemente aqui
listMap f (Node x xs) = Node (f x) (listMap f xs)	


listToStr :: Show a => List a -> [Char]
listToStr xs = listToStr' (listMap show xs)
  where listToStr' Empty = ""
        listToStr' (Node x Empty) = x
        listToStr' (Node x xs) = x ++ ":" ++ listToStr' xs
