module Main (main) where

import LispReader.LispReader (read, makeStringInputStream)

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runState)
import Data.List (elemIndices)
import Prelude hiding (read)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

readProgram :: IO String
readProgram = readProgram' ""
    where
        readProgram' buffer = do
            putStr "* "
            input <- getLine
            let buffer' = if null buffer then input else buffer ++ ('\n' : input)
                numberOfOpen = length $ elemIndices '(' buffer'
                numberOfClose = length $ elemIndices ')' buffer'
            if numberOfClose >= numberOfOpen
                then return buffer'
                else readProgram' buffer'

repLoop :: IO ()
repLoop = do
    src <- readProgram
    case fst $ runState (runExceptT read) (makeStringInputStream src) of
        Right str ->
            putStrLn ("SUCCESS " ++ str) >>
                repLoop

        Left err ->
            putStrLn ("FAILURE " ++ err) >>
                repLoop

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Hisp - A Common Lisp implementation written with Haskell."
    putStrLn "Listening to what you type..."
    putStrLn ""
    repLoop
