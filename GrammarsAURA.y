{
module GrammarsAURA where
import AURALex
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
  int     { TokenInt $$ }
  bool    { TokenBool $$ }
  '+'     { TokenAdd }
  '-'     { TokenSub }
  '*'     { TokenMul }
  '/'     { TokenDiv }
  if      { TokenIf }
  then    { TokenThen }
  else    { TokenElse }
  '('     { TokenLParen }
  ')'     { TokenRParen }


%%

Expr
  : int                  { EInt $1 }
  | bool                 { EBool $1 }
  | Expr '+' Expr        { Add $1 $3 }
  | Expr '*' Expr        { Mul $1 $3 }
  | if Expr then Expr else Expr
                         { If $2 $4 $6 }
  | '(' Expr ')'         { $2 }

{
parseError _ = error "Error de sintaxis"
}
