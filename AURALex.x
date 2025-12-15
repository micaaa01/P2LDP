{
{-# OPTIONS_GHC -w #-}

module AURALex
  ( Token(..)
  , AlexPosn(..)
  , alexScanTokens
  ) where
}

%wrapper "posn"

-- Clases de caracteres
$digit     = 0-9
$alpha     = [a-zA-Z]
$alphanum  = [a-zA-Z0-9]

tokens :-

  -- Espacios
  $white+                         ;

  -- Comentarios
  "--"[^\n]*                      ;

  -- Palabras clave
  let                             { \p s -> TLet }
  in                              { \p s -> TIn }
  if                              { \p s -> TIf }
  then                            { \p s -> TThen }
  else                            { \p s -> TElse }
  fn                              { \p s -> TFn }
  true                            { \p s -> TBool True }
  false                           { \p s -> TBool False }

  -- Operadores aritméticos
  \+                              { \p s -> TPlus }
  \-                              { \p s -> TMinus }
  \*                              { \p s -> TStar }
  \/                              { \p s -> TSlash }

  -- Comparación
  "<="                            { \p s -> TLessEq }
  ">="                            { \p s -> TGreaterEq }
  "=="                            { \p s -> TEq }
  "!="                            { \p s -> TNotEq }
  \<                              { \p s -> TLess }
  \>                              { \p s -> TGreater }

  -- Lógicos
  "&&"                            { \p s -> TAnd }
  "||"                            { \p s -> TOr }
  \!                              { \p s -> TNot }

  -- Símbolos
  \(                              { \p s -> TLParen }
  \)                              { \p s -> TRParen }
  \=                              { \p s -> TEquals }
  "=>"                            { \p s -> TArrow }

  -- Números
  $digit+ (\. $digit+)?           { \p s -> TNumber (read s) }

  -- Identificadores
  $alpha [$alphanum \_]*          { \p s -> TId s }

{
data Token
  = TNumber Int
  | TBool Bool
  | TId String
  | TLet | TIn
  | TIf | TThen | TElse
  | TFn | TArrow
  | TPlus | TMinus | TStar | TSlash
  | TLess | TGreater | TLessEq | TGreaterEq
  | TEq | TNotEq
  | TAnd | TOr | TNot
  | TLParen | TRParen
  | TEquals
  deriving (Eq, Show)
}
