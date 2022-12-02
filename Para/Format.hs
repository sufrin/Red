

--
-- Front end for Kevin Quick's paragrapher <quick@sparq.org>
-- This is a lot better than Gnu fmt, and better still than OS/X fmt
-- but needs some attention


module Main ( main ) where
import Para
import System.Environment
import Data.List

defaultPaths []    = ["/dev/stdin"]
defaultPaths paths = paths

defaultIndentation = ""

-- occurs
(∈) :: Eq a => a -> [a] -> Bool
(∈) = elem

-- included
(⊆) :: Eq a => [a] -> [a] -> Bool
as ⊆ bs = all (∈ bs) as

-- prefixes
(≼) :: Eq a => [a] -> [a] -> Bool
[]     ≼ _      =  True
(x:xs) ≼ (y:ys) = x==y && xs ≼ ys
_      ≼ _      = False

-- drop prefix (if xs≼ys)
(⇓):: [a] -> [a] -> [a]
xs ⇓ ys = length xs `drop` ys

digits arg = arg ⊆ "0123456789"


readArgs:: [String] -> (String, String, [String]) -- (width, drop, paths)
readArgs args =
  pr args ("75", "", [])
  where
  pr []         (width, drop, paths) = (width, drop, defaultPaths $ reverse paths)
  pr (arg:args) (width, drop, paths) =
     if "--prefix=" ≼ arg then
        pr args (width, "--prefix=" ⇓ arg, paths)
     else if "--width=" ≼ arg then
        pr args ("--width=" ⇓ arg, drop, paths)
     else if "-" ≼ arg && digits (tail arg) then
        pr args (tail arg, drop, paths)
     else if digits arg then
        pr args (arg, drop, paths)
     else pr args (width, drop, arg:paths)


--
-- Remove leading spaces and the given prefix from each of the lines in the text
--
dropPrefix prefix text =
           let ls  = lines text
               ls' = map unprefix ls
               unprefix line = if prefix ≼ line' then prefix⇓line' else line'
                        where line' = dropWhile (== ' ') line
           in  unlines ls'

rePrefix prefix lines = map (prefix ++) lines

firstLine = head . lines

main = do args <- getArgs
          let (width, prefix, paths) = readArgs args
          -- putStrLn $ show $ (width, prefix, paths)
          bodies <- mapM readFile $ paths
          let texts       = intersperse "\n" bodies
          let isComment   = False  -- "/**" ≼ (dropWhile (== ' ') $ head texts)
          let preprocess  = if null prefix then
                               (if isComment then dropPrefix "*" else id)
                            else dropPrefix prefix
          let prePrefix   = takeWhile (==' ') $ firstLine  $ head texts
          let postprocess = if null prefix then
                               (if isComment then rePrefix "*" else id)
                            else rePrefix (prePrefix++prefix)
          putStrLn     $
           unlines     $
           postprocess $
           formatParas (read width) (Just defaultIndentation) $
           map preprocess $ texts
