import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs -- Retrieve command line arguments
  case args of
    [fileName, w1, w2] -> do
      input <- readFile fileName -- Read the contents of the file
      let modified = substituteAll w1 w2 input -- Perform word substitution
          outputFileName = "subst-" ++ fileName -- Generate output file name
      writeFile outputFileName modified -- Write modified contents to output file
      putStrLn $ "Substitution completed. Modified content saved in " ++ outputFileName
    _ -> putStrLn "Invalid arguments. Please provide a file name, w1, and w2."

substituteAll :: String -> String -> String -> String
substituteAll w1 w2 = unwords . map (substituteWord w1 w2) . words

substituteWord :: String -> String -> String -> String
substituteWord w1 w2 word
  | word == w1 = w2
  | otherwise = word
