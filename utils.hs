module Utils where

import Data.List
import Data.Char

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

fixSpaces :: String -> String
fixSpaces s = trim $ (head) <$> groupBy (\a b -> a==b && a==' ') s

firstKeysExp :: String -> (String, String)
firstKeysExp s =
    let
        dropUntilOpenKeys ('[':cs) = cs
        dropUntilOpenKeys (_:cs)   = dropUntilOpenKeys cs
        separateUntilCloseKeys (']':cs) (b,a) = (b, trim $ tail a)
        separateUntilCloseKeys (c:cs) (b,_) = separateUntilCloseKeys cs (b++[c], cs)
    in
        separateUntilCloseKeys (dropUntilOpenKeys s) ("", "")

parens :: String -> (String, String)
parens [] = ([],[])
parens s | head s /= '(' = (s,[])
         | otherwise =
    let parens' (p,[]) 0 = (p,[])
        parens' (p,[]) n = error "No coinciden los paréntesis de apertura y de cierre"
        parens' (p,u) n = case head u of
            '(' -> parens' (p++"(",tail u) (n+1)
            ')' -> if n == 0 then (p, trim $ tail u) else parens' (p++")", trim $ tail u) (n-1)
            c   -> parens' (p++[c], tail u) n
    in parens' ("", trim $ tail s) 0

csvLineToList :: String -> [String]
csvLineToList s =
    let
        csvLineToList' (p,[]) = (p,[])
        csvLineToList' (p,u)
            | not $ elem ';' u = (p ++ [u], "")
            | otherwise        = csvLineToList' (p ++ [takeWhile (/=';') u], tail $ dropWhile (/=';') u)
    in trim <$> (fst $ csvLineToList' ([],s))

flistToCsvLine :: (String -> String) -> [String] -> String
flistToCsvLine _ [] = []
flistToCsvLine f ss = foldr1 (\f1 f2 -> f1 ++ ";" ++ f2) $ f <$> ss

listToCsvLine :: [String] -> String
listToCsvLine = flistToCsvLine (id)
