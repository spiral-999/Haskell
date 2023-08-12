-- IDENTIFICAÇÃO

atividade = 7

nome = "Mateus Lima Rodrigues"

matricula = "536334"

data Poly = Poly [Float]
instance Show Poly where
    show (Poly []) = ""
    show (Poly (c:cs)) = showTerm c 0 ++ showTerms cs 1
        where
            showTerm :: Float -> Int -> String
            showTerm c p
                | p == 0 = show c
                | c == 1 = "x^" ++ show p
                | c == -1 = "-x^" ++ show p
                | otherwise = show c ++ "x^" ++ show p

            showTerms :: [Float] -> Int -> String
            showTerms [] _ = ""
            showTerms (c:cs) p
                | c >= 0 = "+" ++ showTerm c p ++ showTerms cs (p + 1)
                | otherwise = showTerm c p ++ showTerms cs (p + 1)

avalPoly :: Poly -> Float -> Float
avalPoly (Poly cs) x = sum $ zipWith (\c p -> c * x ** p) cs [0..]

