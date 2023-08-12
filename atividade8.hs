--IDENTIFICAÇÃO

--atividade = 8
--matricula = "536334"
--nome      = "Mateus Lima Rodrigues"

-- CÓDIGO

import System.IO
import System.Environment

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find repl s@(x:xs)
    | takeFind s find == find = repl ++ replace find repl (dropFind (length find) s)
    | otherwise = x : replace find repl xs
  where
    takeFind :: [a] -> [a] -> [a]
    takeFind _ [] = []
    takeFind [] _ = []
    takeFind (y:ys) (_:bs) = y : takeFind ys bs

    dropFind :: Int -> [a] -> [a]
    dropFind 0 xs = xs
    dropFind _ [] = []
    dropFind n (_:xs) = dropFind (n - 1) xs

main :: IO ()
main = do
    args <- getArgs
    let inputFile = args !! 0
        word1 = args !! 1
        word2 = args !! 2
        outputFile = "subst-" ++ inputFile

    content <- readFile inputFile
    let newContent = replace word1 word2 content

    writeFile outputFile newContent
    putStrLn $ "Arquivo '" ++ outputFile ++ "' criado com sucesso."