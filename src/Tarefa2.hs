module Main where
import Data.Char (isDigit)
import System.Environment
import Data.Char
import T1Lib 
import T2Lib

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"

