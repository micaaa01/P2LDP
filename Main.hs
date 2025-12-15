module Main where

import AURALex (alexScanTokens)
import GrammarsAURA (parseExpr)
import AlgoritmoW (inferTypeEmpty, prettyType)

import System.IO (hFlush, stdout)

-- Inferir tipo a partir de un string
inferFromString :: String -> String
inferFromString input =
  let tokens = alexScanTokens input
      ast    = parseExpr tokens
      ty     = inferTypeEmpty ast
  in prettyType ty

-- REPL mínima
repl :: IO ()
repl = do
  putStr ">> "
  hFlush stdout
  input <- getLine
  if input == "salir"
    then putStrLn "Bye"
    else do
      putStrLn ("Tipo inferido: " ++ inferFromString input)
      repl

main :: IO ()
main = do
  putStrLn "Inferencia de tipos Algoritmo W"
  putStrLn "Escribe una expresión o escribe salir para terminar con la ejecución: "
  repl
