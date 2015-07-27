module Main where

import SimpleJSON
import PrettyJSON
import Prettify

main = putStrLn . pretty 10 . renderJValue $ JObject [("foo", JNumber 1), ("bar", JBool False)]

