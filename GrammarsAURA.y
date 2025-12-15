{
module GrammarsAURA where

import AURALex (Token(..), AlexPosn(..))
import AST
}

-- Nombre del parser
%name parseExpr Expr
%name parseProgram Program

-- Tipo de los tokens
%tokentype { Token }

-- Tipo de error
%error { parseError }

-- Definición de tokens
%token
    -- Literales
    number      { TNumber $$ }
    id          { TId $$ }
    true        { TBool True }
    false       { TBool False }
  
    
    -- Palabras clave
    let         { TLet }
    in          { TIn }
    if          { TIf }
    then        { TThen }
    else        { TElse }
    fn          { TFn }
    
    -- Operadores aritméticos
    '+'         { TPlus }
    '-'         { TMinus }
    '*'         { TStar }
    '/'         { TSlash }
    
    -- Operadores de comparación
    '<'         { TLess }
    '>'         { TGreater }
    '<='        { TLessEq }
    '>='        { TGreaterEq }
    '=='        { TEq }
    '!='        { TNotEq }
    
    -- Operadores lógicos
    '&&'        { TAnd }
    '||'        { TOr }
    '!'         { TNot }
    
    -- Símbolos
    '('         { TLParen }
    ')'         { TRParen }
    '='         { TEquals }
    '=>'        { TArrow }

-- Precedencia y asociatividad (de menor a mayor precedencia)
%nonassoc '==' '!='
%nonassoc '<' '>' '<=' '>='
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/'
%nonassoc NEG NOT         -- Operadores unarios
%left APP                 -- Aplicación de función

%%

-- Reglas de producción

Program :: { Expr }
Program : Expr                          { $1 }

Expr :: { Expr }
Expr : LogicalOrExpr                    { $1 }

-- Nivel 1: Disyunción lógica (||)
LogicalOrExpr :: { Expr }
LogicalOrExpr
    : LogicalOrExpr '||' LogicalAndExpr { EBinOp Or $1 $3 }
    | LogicalAndExpr                    { $1 }

-- Nivel 2: Conjunción lógica (&&)
LogicalAndExpr :: { Expr }
LogicalAndExpr
    : LogicalAndExpr '&&' EqualityExpr  { EBinOp And $1 $3 }
    | EqualityExpr                      { $1 }

-- Nivel 3: Igualdad (==, !=)
EqualityExpr :: { Expr }
EqualityExpr
    : ComparisonExpr '==' ComparisonExpr { EBinOp Equal $1 $3 }
    | ComparisonExpr '!=' ComparisonExpr { EBinOp NotEqual $1 $3 }
    | ComparisonExpr                     { $1 }

-- Nivel 4: Comparación (<, >, <=, >=)
ComparisonExpr :: { Expr }
ComparisonExpr
    : AdditiveExpr '<' AdditiveExpr     { EBinOp Less $1 $3 }
    | AdditiveExpr '>' AdditiveExpr     { EBinOp Greater $1 $3 }
    | AdditiveExpr '<=' AdditiveExpr    { EBinOp LessEq $1 $3 }
    | AdditiveExpr '>=' AdditiveExpr    { EBinOp GreaterEq $1 $3 }
    | AdditiveExpr                      { $1 }

-- Nivel 5: Suma y resta (+, -)
AdditiveExpr :: { Expr }
AdditiveExpr
    : AdditiveExpr '+' MultiplicativeExpr { EBinOp Add $1 $3 }
    | AdditiveExpr '-' MultiplicativeExpr { EBinOp Sub $1 $3 }
    | MultiplicativeExpr                  { $1 }

-- Nivel 6: Multiplicación y división (*, /)
MultiplicativeExpr :: { Expr }
MultiplicativeExpr
    : MultiplicativeExpr '*' UnaryExpr  { EBinOp Mul $1 $3 }
    | MultiplicativeExpr '/' UnaryExpr  { EBinOp Div $1 $3 }
    | UnaryExpr                         { $1 }

-- Nivel 7: Operadores unarios (-, !)
UnaryExpr :: { Expr }
UnaryExpr
    : '-' UnaryExpr  %prec NEG          { EUnOp Neg $2 }
    | '!' UnaryExpr  %prec NOT          { EUnOp Not $2 }
    | ApplicationExpr                   { $1 }

-- Nivel 8: Aplicación de función (mayor precedencia)
ApplicationExpr :: { Expr }
ApplicationExpr
    : ApplicationExpr AtomicExpr %prec APP { EApp $1 $2 }
    | AtomicExpr                           { $1 }

-- Expresiones atómicas
AtomicExpr :: { Expr }
AtomicExpr
    : number                            { ENum $1 }
    | true                              { EBool True }
    | false                             { EBool False }
    | id                                { EVar $1 }
    | IfExpr                            { $1 }
    | LetExpr                           { $1 }
    | LambdaExpr                        { $1 }
    | '(' Expr ')'                      { $2 }

-- If-then-else
IfExpr :: { Expr }
IfExpr
    : if Expr then Expr else Expr       { EIf $2 $4 $6 }

-- Let binding
LetExpr :: { Expr }
LetExpr
    : let id '=' Expr in Expr           { ELet $2 $4 $6 }

-- Lambda (función anónima)
LambdaExpr :: { Expr }
LambdaExpr
    : fn id '=>' Expr                   { ELam $2 $4 }
    | fn id '=>' LambdaExpr             { ELam $2 $4 }  -- Múltiples parámetros


{
-- Función de error
parseError :: [Token] -> a
parseError tokens = error $ "Parse error at: " ++ show (take 10 tokens)

}
