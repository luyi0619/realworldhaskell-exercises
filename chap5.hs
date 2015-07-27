import SimpleJSON
import PrettyJSON
import Prettify

-- 5.1 Write a function, fill, with the following type signature. It should add spaces to a document until it is the given number of columns wide. If it is already wider than this value, it should add no spaces.

fill :: Int -> Doc -> Doc
fill width x = fill' 0 [x]
    where fill' col (d:ds) = 
            case d of
                Empty           -> d <> fill' col ds
                Char c          -> d <> fill' (col + 1) ds
                Text s          -> d <> fill' (col + length s) ds
                Line            -> space col <> Line <> fill' 0 ds
                a `Concat` b    -> fill' col (a:b:ds)
                a `Union` b     -> fill' col (a:ds) `Union` fill' col (b:ds)
          fill' col [] = space col 
          space col | col <= width = Text (replicate (width - col) ' ') 
                    | otherwise = empty
                    
                    
-- 5.2 Our pretty printer does not take nesting into account. Whenever we open parentheses, braces, or brackets, any lines that follow should be indented so that they are aligned with the opening character until a matching closing character is encountered. Add support for nesting, with a controllable amount of indentation.

nest :: Int -> Doc -> Doc
nest width x = nest' 0 [x] [0]
    where nest' col (d:ds) space = 
            case d of
                Empty           -> d <> nest' col ds space
                Char c          | open c -> d <> nest' (col + width) (line:ds) (col: space)
                                | close c -> line <> Text(replicate (head space) ' ') <> d <> nest' (1 + head space ) ds (tail space)
                                | c == ',' -> d <> nest' (head space + width - 1) (line:ds) space
                                | otherwise -> d <> nest' (col+1) ds space
                Text s          -> d <> nest' (col + length s) ds space
                Line            -> d <> Text(replicate col ' ') <> nest' col ds space
                a `Concat` b    -> nest' col (a:b:ds) space
                a `Union` b     -> nest' col (a:ds) space `Union` nest' col (b:ds) space
          nest' _ _ _  = empty
          open  c = c `elem` "([{"
          close c = c `elem` ")]}"