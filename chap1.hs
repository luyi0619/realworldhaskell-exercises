-- 1.1 Enter the following expressions into ghci. What are their types?

5 + 8 :: Num a => a

3 * 5 + 8 :: Num a => a

2 + 4 :: Num a => a

(+) 2 4 :: Num a => a

sqrt 16 :: Floating a => a

succ 6 :: (Enum a, Num a) => a

succ 7 :: (Enum a, Num a) => a

pred 9 :: (Enum a, Num a) => a

pred 8 :: (Enum a, Num a) => a

sin (pi / 2) :: Floating a => a

truncate pi :: Integral b => b

round 3.5 :: Integral b => b

round 3.4 :: Integral b => b

floor 3.7 :: Integral b => b

ceiling 3.3 :: Integral b => b

-- 1.2 From ghci, type :? to print some help. Define a variable, such as let x = 1, then type :show bindings. What do you see?

x :: Num a => a = _

-- 1.3 The words function counts the number of words in a string. Modify the WC.hs example to count the number of words in a file.

main = interact wordCount
    where wordCount input = show (length (words input)) ++ "\n"

-- 1.4 Modify the WC.hs example again, to print the number of characters in a file.

main = interact charCount
    where charCount input = show (length input) ++ "\n"