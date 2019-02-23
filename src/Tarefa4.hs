module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import T4Lib
import T1Lib

main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"
