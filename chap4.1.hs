-- 4.1.1 Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail. As a hint, you might want to consider using the following types. 

safeHead :: [a] -> Maybe a

safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]

safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a

safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]

safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) =  case safeInit xs of
               Just xs' -> Just(x : xs')
               Nothing -> Just [x]
               
-- 4.1.2 Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False.

splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith pred [] = []
splitWith pred xs = let npred = not . pred
                    in let (pre, suf) = break npred xs
                        in case pre of 
                            [] ->  splitWith pred (dropWhile npred suf)
                            xs -> [xs] ++ splitWith pred (dropWhile npred suf)

-- 4.1.3 Using the command framework from the section called “A simple command line framework”, write a program that prints the first word of each line of its input. 

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = \z -> map (\x -> takeWhile (\y -> y /= ' ') x) ( lines z )
        
-- 4.1.4 Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = unlines . trans . lines


minLen = foldr (\x acc -> if acc == -1 then length x else min (length x) acc) (-1)

transpose n input | n == 0 = [] 
                  | n /= 0 = (map head input) :  (transpose (n - 1) (map tail input))
                  
trans xs = transpose (minLen xs) xs