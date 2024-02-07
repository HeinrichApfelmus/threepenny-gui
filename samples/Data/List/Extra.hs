module Data.List.Extra where

import Data.Char
    ( isSpace
    )

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
