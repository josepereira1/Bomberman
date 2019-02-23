module Main where
import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import T1Lib
  
main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"


{-|
As funções da __Tarefa 1__, estão num ficheiro, para que pudesse as funções desta Tarefa nas outras Tarefas, criando assim 
um biblioteca minha denominada T1Lib.
-}