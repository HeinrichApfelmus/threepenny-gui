module Data.List.Extra where
                  
import Data.List
import Data.Char
import Data.Maybe

unionOf :: (Eq a) => [[a]] -> [a]
unionOf = foldr union []

lastToMaybe :: [a] -> Maybe a
lastToMaybe [x]    = Just x
lastToMaybe (_:xs) = lastToMaybe xs
lastToMaybe []     = Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Return the first item of a list or something else.
firstOr :: a -> [a] -> a
firstOr n = fromMaybe n . listToMaybe

maxList [] = 0
maxList xs = maximum xs

for = flip map