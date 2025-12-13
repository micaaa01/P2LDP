{
{-# OPTIONS_GHC -w #-}

module Lexer 
    ( Token(..)
    , AlexPosn(..)
    , alexScanTokens
    , scanTokens
    ) where

import Data.Char (ord)
}

%wrapper "posn"

-- Definición de caracteres
$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]

-- Definición de tokens
tokens :-

  -- Espacios en blanco (ignorar)
  $white+                               ;
  
  -- Comentarios (opcional)
  "--".*                                ;
  
  -- Palabras clave
  let                                   { \p s -> TLet }
  in                                    { \p s -> TIn }
  if                                    { \p s -> TIf }
  then                                  { \p s -> TThen }
  else                                  { \p s -> TElse }
  fn                                    { \p s -> TFn }
  true                                  { \p s -> TBool True }
  false                                 { \p s -> TBool False }
  sqrt                                  { \p s -> TSqrt }
  
  -- Operadores aritméticos
  \+                                    { \p s -> TPlus }
  \-                                    { \p s -> TMinus }
  \*                                    { \p s -> TStar }
  \/                                    { \p s -> TSlash }
  \^                                    { \p s -> TPower }
  
  -- Operadores de comparación
  "<="                                  { \p s -> TLessEq }
  ">="                                  { \p s -> TGreaterEq }
  "=="                                  { \p s -> TEq }
  "!="                                  { \p s -> TNotEq }
  \<                                    { \p s -> TLess }
  \>                                    { \p s -> TGreater }
  
  -- Operadores lógicos
  "&&"                                  { \p s -> TAnd }
  "||"                                  { \p s -> TOr }
  \!                                    { \p s -> TNot }
  
  -- Símbolos
  \(                                    { \p s -> TLParen }
  \)                                    { \p s -> TRParen }
  \=                                    { \p s -> TEquals }
  "=>"                                  { \p s -> TArrow }
  
  -- Números (enteros y decimales)
  $digit+ (\. $digit+)?                 { \p s -> TNumber (read s) }
  
  -- Identificadores
  $alpha [$alphanum \_]*                { \p s -> TId s }

{
-- Definición de tokens
data Token
    = TNumber Double
    | TBool Bool
    | TId String
    | TLet
    | TIn
    | TIf
    | TThen
    | TElse
    | TFn
    | TArrow
    | TPlus
    | TMinus
    | TStar
    | TSlash
    | TPower
    | TSqrt
    | TLess
    | TGreater
    | TLessEq
    | TGreaterEq
    | TEq
    | TNotEq
    | TAnd
    | TOr
    | TNot
    | TLParen
    | TRParen
    | TEquals
    | TEOF
    deriving (Eq, Show)

-- Posición en el código fuente
data AlexPosn = AlexPn !Int !Int !Int
    deriving (Eq, Show)

-- Función para escanear tokens
scanTokens :: String -> [Token]
scanTokens str = go (alexStartPos, '\n', [], str)
  where
    go inp@(pos, _, _, str) =
      case alexScan inp 0 of
        AlexEOF -> []
        AlexError ((AlexPn _ line col), _, _, _) -> 
            error $ "Lexical error at line " ++ show line ++ ", column " ++ show col
        AlexSkip  inp' _     -> go inp'
        AlexToken inp' len act -> act pos (take len str) : go inp'

-- Posición inicial
alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

-- Para compatibilidad con el wrapper "posn"
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (p, c, bs, s) = case s of
    []  -> Nothing
    (c:s) -> let p' = alexMove p c
                 (b:bs') = utf8Encode c
             in Just (b, (p', c, bs', s))

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1) l (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1) 1
alexMove (AlexPn a l c) _    = AlexPn (a+1) l (c+1)

type AlexInput = (AlexPosn,     -- posición actual
                  Char,         -- carácter anterior
                  [Byte],       -- bytes pendientes
                  String)       -- entrada actual

type Byte = Word8

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
    go oc
      | oc <= 0x7f   = [oc]
      | oc <= 0x7ff  = [ 0xc0 + (oc `shiftR` 6)
                       , 0x80 + oc .&. 0x3f
                       ]
      | oc <= 0xffff = [ 0xe0 + (oc `shiftR` 12)
                       , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                       , 0x80 + oc .&. 0x3f
                       ]
      | otherwise    = [ 0xf0 + (oc `shiftR` 18)
                       , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                       , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                       , 0x80 + oc .&. 0x3f
                       ]

import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))
}
