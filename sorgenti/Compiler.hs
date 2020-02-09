module Compiler (
  compileFile,
  compileSource
                ) where

import Parser
import TypeChecker
import TAC
import PrettyPrinting
import Data.List as L
import Data.Vector as V

-- Front-End entry points

compileFile :: FilePath -> IO ()
compileFile filepath = do
  res  <- parseFile filepath
  case res of
    Left str -> putStrLn str
    Right ast -> do
      tc <- typeCheckProgram ast
      case tc of
        Left e -> do
          source <- readFile filepath
          let fc = V.map V.fromList . V.fromList $ lines source
          printErrors fc e
        Right a -> do
          putStrLn "Pretty print AST:"
          putStrLn $ toString a
          putStrLn "Pretty print TAC:"
          tac <- codeGenTAC a
          putStrLn $ prettyPrintTAC tac

compileSource :: String -> IO ()
compileSource source = do
  res <- parseStmts source
  case res of
    Left str -> putStrLn str
    Right ast -> do
      tc <- typeCheckProgram ast
      case tc of
        Left e -> do
          let fc = V.map V.fromList . V.fromList $ lines source
          printErrors fc e
        Right a -> do
          putStrLn "\nPretty print AST:"
          putStrLn $ toString a
          putStrLn "\nPretty print TAC:"
          tac <- codeGenTAC a
          putStrLn $ prettyPrintTAC tac
