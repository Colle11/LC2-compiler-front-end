
module Main where

import Compiler
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            str <- getContents
            compileSource str

        y@(x:xs) -> do
          mapM_ compileFile y
