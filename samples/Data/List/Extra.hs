module Data.List.Extra where
                  
import Data.List
import Data.Char
import Data.Maybe

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
