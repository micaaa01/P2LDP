{
module AURALex where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $digit+        { \s -> TokenInt (read s) }
  "true"         { \_ -> TokenBool True }
  "false"        { \_ -> TokenBool False }
  "+"            { \_ -> TokenPlus }
  "*"            { \_ -> TokenMul }
  "if"           { \_ -> TokenIf }
  "then"         { \_ -> TokenThen }
  "else"         { \_ -> TokenElse }
  "("            { \_ -> TokenLParen }
  ")"            { \_ -> TokenRParen }
  [ \t\n\r]+     ;

{
data Token
  = TokenInt Int
  | TokenBool Bool
  | TokenPlus
  | TokenMul
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenLParen
  | TokenRParen
  deriving Show
}
