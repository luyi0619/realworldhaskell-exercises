-- 4.2.1 Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function from the section called “Explicit recursion”.
import Data.Char (digitToInt, isDigit, isSpace)

asInt_fold :: String -> Int

asInt_fold ('-':xs) = -(asInt_fold xs)
asInt_fold xs = foldl step 0 xs
        where step acc x  
                | isDigit x = acc * 10 + digitToInt x
                | otherwise = error "Char.digitToInt: not a digit '.'"

-- 4.2.2 The asInt_fold function uses error, so its callers cannot handle errors. Rewrite it to fix this problem.

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int

asInt_either ('-':xs) = case asInt_either xs of 
                    Right acc' -> Right (- acc')
                    Left err -> Left err

asInt_either xs = foldl step (Right 0) xs
        where step acc x
                   |  isDigit x = case acc of 
                            Right acc' ->  Right (acc' * 10 + digitToInt x)
                            Left err -> Left err
                   |  otherwise = Left [x]


-- 4.2.3 The Prelude function concat concatenates a list of lists into a single list, and has the following type. Write your own definition of concat using foldr.

concat' :: [[a]] -> [a]
concat' = foldr (++) [] 

-- 4.2.4 Write your own definition of the standard takeWhile function, first using explicit recursion, then foldr.

takeWhile' f [] = []
takeWhile' f (x:xs) 
            | f x = x : (takeWhile' f xs)
            | otherwise = []
            
takeWhile'' f = foldr step []
        where step x acc 
                | f x = x : acc
                | otherwise = []
                

-- 4.2.5 The Data.List module defines a function, groupBy, which has the following type. Use ghci to load the Data.List module and figure out what groupBy does, then write your own implementation using a fold.

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]

groupBy' f = foldr step []
        where step x acc 
                |  null acc = [x] : acc
                |  otherwise = let lastg = head acc
                               in  let lastx = head lastg
                                    in if f x lastx then (x : lastg) : (tail acc) else [x] : acc 


-- 4.2.6 How many of the following Prelude functions can you rewrite using list folds? any, cycle, words, unlines. For those functions where you can use either foldl' or foldr, which is more appropriate in each case?


any' f = foldl (\acc x -> acc || (f x) ) False 

cycle' [] = error "empty list"
cycle' xs = foldr step [] [1..]
                where step x acc = xs ++ acc

words' :: String -> [String]

words' = foldr step [[]]
            where step x acc
                    | isSpace x = if null (head acc) then acc else [] : acc 
                    | otherwise = let lastword = head acc
                                  in  (x : lastword) : (tail acc)

unlines' :: [String] -> String

unlines' = foldr (\x acc -> x ++ "\n" ++ acc) []
 
