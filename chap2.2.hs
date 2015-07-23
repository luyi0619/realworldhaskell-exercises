-- 2.2.1 Haskell provides a standard function, last :: [a] -> a, that returns the last element of a list. From reading the type alone, what are the possible valid behaviours (omitting crashes and infinite loops) that this function could have? What are a few things that this function clearly cannot do?

-- 2.2.2 Write a function lastButOne, that returns the element before the last.

lastButOne [] = error ("length of array should be at least 2")
lastButOne [x] = error ("length of array should be at least 2")
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs

-- 2.2.3 Load your lastButOne function into ghci, and try it out on lists of different lengths. What happens when you pass it a list that's too short?