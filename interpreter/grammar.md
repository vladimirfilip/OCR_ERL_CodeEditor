# Grammar

Program -> ProgramBlock Program | ε

ProgramBlock -> InstrBlock
                | FunDecl
                | ProcDecl
                | ClassDecl

InstrBlock -> Instr InstrBlock'
InstrBlock' -> Instr InstrBlock' | ε

## Fun declarations

FunDecl -> 'function' ID '(' ParamList ')' FunInstrBlock 'endfunction'

FunInstrBlock -> InnerInstr FunInstrBlock | ε

## Proc declarations

ProcDecl -> 'procedure' ID '(' ParamList ')' ProcInstrBlock 'endprocedure'

ProcInstrBlock -> InnerInstr ProcInstrBlock | ε

## Class declarations

ClassDecl -> 'class' ID ClassInh
                 ClassBlock
             'endclass'

ClassInh -> 'inherits' ID | ε

ClassBlock -> ClassMember ClassBlock' // at least one MemberDecl required

ClassBlock' -> ClassMember | ε

ClassMember -> AccessModifier ClassMember'

AccessModifier -> 'public' | 'private' | ε

ClassMember' -> AttrDecl | FunDecl | ProcDecl

AttrDecl -> ArrayDecl | ID MemberInit

MemberInit -> '=' Expr | ε

## Instructions

Instr -> GlobDecl | ArrOrVar
         | IfElse | ForLoop | WhileLoop | DoUntil | SwitchCase
         | ProcCall | FunCall
         | PrintInstr | ReturnInstr

GlobDecl -> 'global' ArrOrVar | ε

ArrOrVar -> ArrayDecl | VarAssign

ArrayDecl -> 'array' ID '[' ExprList ']'

VarAssign -> LhsAddrExpr AddrAssign

AddrAssign -> '=' Expr

OptionalExprList -> ExprList | ε

### Inner instructions

InnerInstrBlock -> InnerInstr InnerInstrBlock | ε

InnerInstr -> Instr | GoToInstr

GoToInstr -> 'break' | 'continue'

### If-Else

IfElse -> 'if' Expr 'then' InnerInstrBlock ElseIf 'endif'
ElseIf -> 'elseif' Expr 'then' InnerInstrBlock ElseIf 
          | Else 
          | ε
Else -> 'else' InnerInstrBlock

### Switch

SwitchCase -> 'switch' Expr ':' Cases Default 'endswitch'

Cases -> Case Cases | ε
Case -> 'case' Expr ':' InnerInstrBlock

Default -> 'default' ':' InnerInstrBlock | ε

### For

ForLoop -> 'for' ID '=' Expr 'to' Expr
               InnerInstrBlock
           'next' ID

### While

WhileLoop -> 'while' Expr
                 InnerInstrBlock
             'endwhile'

### Do-Until
DoUntil -> 'do'
               InnerInstrBlock
           'until' Expr

### Parameters

ParamList -> Param ParamList' | ε

ParamList' -> ',' Param ParamList' | ε

Param -> ID ParamPassing

ParamPassing -> ':byRef' | ':byVal' | ε

## Expressions

### Address expressions

AddrExpr -> AddrMember AddrExpr'

AddrExpr' -> '.' (AddrMember AddrExpr' 
                    | BuiltInAttribute)
            | ε

AddrMember -> AddrIdOrCall IndexingSuffix

AddrIdOrCall -> ID CallableSuffix

IndexingSuffix -> '[' ExprList ']' | ε

CallableSuffix -> '(' OptionalExprList ')' | ε

### Calls

FunCall -> AddrInstr

ProcCall -> AddrInstr // verify type at runtime

ExprList -> Expr ExprList'

ExprList' -> ',' Expr ExprList' | ε



### Operator expressions

Expr -> Term Expr'

Expr' -> AddOp Term Expr' | ε

Term -> Factor Term'

Term' -> MulOp Factor Term' | ε

Factor -> SimpleExpr Factor'

Factor' -> PowOp SimpleExpr Factor' | ε

SimpleExpr -> '-' SimpleExpr
        | 'NOT' SimpleExpr
        | '(' Expr ')'
        | AddrExpr
        | NewExpr
        | FunExpr
        | NUM | INT | STRING

NewExpr -> 'new' ID '(' ExprList ')'


AddOp -> '+' | '-' | 'AND'   
MulOp -> '*' | '/' | 'OR' | 'MOD' | 'DIV'
PowOp -> '^' | '==' | '>' | '>=' | '<' | '<=' | '!=' 

### Built-in functions

FunExpr -> CastInt
        | CastString
        | CastFloat
        | Input
        | OpenRead
        | OpenWrite

CastInt -> 'int' '(' Expr ')'
CastString -> 'str' '(' Expr ')'
CastFloat -> 'float' '(' Expr ')'
Input -> 'input' '(' Expr ')'
OpenRead -> 'openRead' '(' Expr ')'
OpenWrite -> 'openWrite' '(' Expr ')'

BuiltInAttribute -> AddrExpr '.' (StrLen 
                                    | StrSubstring
                                    | EndOfFile
                                    | ReadLine
                                    | WriteLine
                                    | Close )

StrLen -> 'length'
StrSubstring -> 'substring' '(' Expr ',' Expr ')'
EndOfFile -> 'endOfFile' '(' ')'
ReadLine -> 'readLine' '(' ')'
WriteLine -> 'writeLine' '(' Expr ')'
Close -> 'close' '(' ')'

PrintInstr -> 'print' '(' PrintInstrArgs ')'

PrintInstrArgs -> Expr PrintInstrArgs'

PrintInstrArgs' -> ( ',' Expr PrintInstrArgs' ) | ε
