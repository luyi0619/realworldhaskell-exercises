-- 3.1.1 Write the converse of fromList for the List type: a function that takes a List a and generates a [a].

data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList Nil = []
toList (Cons x xs) = x : toList xs


-- 3.1.2 Define a tree type that has only one constructor, like our Java example. Instead of the Empty constructor, use the Maybe type to refer to a node's children.

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)
