import Data.List (sortBy)
import Prelude hiding (Right, Left)

-- 3.2.1 Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function. 

length' [] = 0
length' (x:xs) = 1 + length' xs

-- 3.2.2 Add a type signature for your function to your source file. To test it, load the source file into ghci again

length' :: [a] -> Int

-- 3.2.3 Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)

mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- 3.2.4 Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].

toPalindrome xs = xs ++ reverse xs

-- 3.2.5 Write a function that determines whether its input list is a palindrome.

isPalindrome xs = take len xs == take len (reverse xs) where len = length xs `div` 2

-- 3.2.6 Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)

sort' xs = sortBy comp xs
     where comp a b
            | length a < length b = LT
            | length a > length b  = GT
            | otherwise = EQ

-- 3.2.7 Define a function that joins a list of lists together using a separator value. 

intersperse' sep [] = []
intersperse' sep (x:[]) = x
intersperse' sep (x:xs) = x ++ (sep: (intersperse' sep xs))

-- 3.2.8 Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an Empty. For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
              
height Empty = 0
height (Node a left right) = 1 + max (height left) (height right)

-- 3.2.9 Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities. 25 comments

data Direction = Right
               | Left
               | Line
                 deriving (Show, Eq)

-- 3.2.10 Write a function that calculates the turn made by three 2D points and returns a Direction.

data Point = Point {
          x :: Double, 
          y :: Double
        } deriving (Show, Eq)

ccw p1 p2 p3 | value > 0 = Left 
             | value < 0 = Right 
             | otherwise = Line
          where value = (x p2 - x p1) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)

-- 3.2.11 Define a function that takes a list of 2D points and computes the direction of each successive triple. Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction. 1

succ_direction xs | length xs < 3 = error ("length < 3")
succ_direction [a, b, c] = [ccw a b c]
succ_direction (a:b:c:xs) = ccw a b c : succ_direction (b:c:xs)


-- 3.2.12 Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull. is, and how the Graham scan algorithm should work, on Wikipedia.

dist p1 p2 = (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

sortAngle p xs = sortBy comp xs
     where comp a b
            | ccw p a b == Right = GT
            | ccw p a b == Left  = LT
            | otherwise = compare (dist p a) (dist p b)

graham_scan xs | length xs < 3 = error ("lenght < 3")
graham_scan xs = scan (tail rem) [head rem, p]
                 where p = foldl (\acc e -> if y e < y acc || (y e == y acc && x e < x acc) then e else acc) (head xs) (tail xs) 
                       rem = (sortAngle p . filter (/= p)) xs
                       scan [] acc = acc
                       scan now@(x:xs) acc@(x':x'': _) | ccw x'' x' x /= Right = scan xs (x:acc)
                                                       | otherwise = scan now (tail acc)
