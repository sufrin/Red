---
--- Front end for Kevin Quick's paragrapher <quick@sparq.org>
--- (Much better than fmt, etc)
---


module Main ( main ) where
import Para
import System.Environment
import Data.List


defaultPaths []    = ["/dev/stdin"]
defaultPaths paths = paths

defaultIndentation = ""

(∈) :: Eq a => a -> [a] -> Bool
(∈) = elem

(⊆) :: Eq a => [a] -> [a] -> Bool
as ⊆ bs = all (∈ bs) as

isWidth arg = arg ⊆ "0123456789"


processArgs:: [String] -> (String, [String]) -- (width, paths)
processArgs args =
  pr args ("75", [])
  where
  pr []         (width, paths) = (width, defaultPaths $ reverse paths)
  pr (arg:args) (width, paths) =
     if isWidth arg then pr args (arg, paths) else pr args (width, arg:paths)


main = do
         --givenArgs <- getArgs
         --let args  = if null givenArgs then defaultArgs else givenArgs
         -- putStrLn $ unlines $ "Format:":args
         -- let width = head args
         -- let paths = tail args
         args <- getArgs
         let (width, paths) = processArgs args
         bodies <- mapM readFile $ paths
         putStrLn $
          unlines $
          formatParas (read width) (Just defaultIndentation) $
          intersperse "\n" bodies
