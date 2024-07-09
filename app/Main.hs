{-# LANGUAGE LambdaCase #-}

module Main (main) where

import LispReader.Internal (internalRead)
import TypeSystem.LispData (LispData(..))

import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Char8 (pack)
import Data.List (elemIndices)
import Prelude hiding (read)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.IO.Streams (fromByteString)

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
    stream <- fromByteString (pack src)

    runExceptT (internalRead stream True NIL False) >>= \case
        Right dat ->
            putStrLn ("SUCCESS " ++ show dat)

        Left err ->
            putStrLn ("FAILURE " ++ show err)

    repLoop

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Hisp - A Common Lisp implementation written with Haskell."
    putStrLn "Listening to what you type..."
    putStrLn ""
    repLoop
