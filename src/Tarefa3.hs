module Main where
import Data.Char (isDigit)
import System.Environment
import Data.Char
import T3Lib
import T1Lib

{-|
==Descrição:
Nesta Tarefa temos como objetivo fazer a função encode e decode, que comprimem e descomprimem respectivamente.
-}           

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"

