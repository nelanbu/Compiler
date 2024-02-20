%{
    open Ast
%}


%token PLUS  "+"
%token MINUS "-"
%token TIMES "*"
%token DIV   "/"
%token MOD  "%"
%token LESSTHAN "<"
%token GREATERTHAN ">"
%token LESSEQUAL "<="
%token GREATEREQUAL ">="
%token EQUALTO "=="
%token NOTEQUAL "!="
%token BITWISEAND "&"
%token BITWISEOR "|"
%token LOGICALAND "&&"
%token LOGICALOR "||"
%token SHIFTLEFT "<<"
%token SHIFTRIGHT ">>"


%token LPAREN "("
%token RPAREN ")"
%token LBRACKET "["
%token RBRACKET "]"
%token LCURBRACKET "{"
%token RCURBRACKET "}"

%token SEMICOL ";"
%token COMMA ","
%token DOT "."
%token LOGNOT "!"
%token BITNOT "~"

%token EQUAL "=" 
%token INCREMENT "++"
%token DECREMENT "--"

%token SINGLEQUOTE "\'"

%token BACKSLASH "\\"

%token TYPEINT
%token TYPECHAR

(*Keyword tokens:*)
%token BREAK
%token DELETE
%token IF
%token ELSE
%token EXTERN
%token NEW
%token RETURN
%token FOR
%token WHILE
%token STRUCT
%token VOID

(*Identifier token:*)
%token <string> IDENTIFIER

(*Types: *)
%token <string> STRING
%token <char> CHAR
%token <int> UINT
%token <int> HEX

%token EOP (*end of program*)

%left "||"
%left "&&"
%left "|"
%left "&"
%left "==" "!="
%left "<" ">" "<=" ">="
%left ">>" "<<"
%left "+" "-"
%left "*" "/" "%"
%left "!" "~"
%nonassoc UMINUS



%start main
%type <p> main

%%


/* main:
  | e = global
    { e }
 */

main:
  | e = program
    { e }

program:
  | e1 = list(global) EOP
    { Program(e1)}
  


stmt:
| e = varassign SEMICOL
    { e }
| e1 = LCURBRACKET e2 = stmtlist e3 = RCURBRACKET 
  { SScope(e2) }

| e1 = IF e2 = LPAREN e3 = expr e4 = RPAREN e5 = stmt
  { SIf(e3, e5, None) } 
| e1 = IF e2 = LPAREN e3 = expr e4 = RPAREN e5 = stmt e6 = ELSE e7 = stmt
  { SIf(e3, e5, Some e7) }

| e1 = WHILE e2 = LPAREN e3 = expr e4 = RPAREN e5 = stmt
  { SWhile(e3, e5) }
| e1 = BREAK e2 = SEMICOL
  { SBreak }
| e1 = RETURN e2 = SEMICOL
  { SReturn(None) }
| e1 = RETURN e2 = expr e3 = SEMICOL
  { SReturn(Some e2) }
| e1 = DELETE e2 = LBRACKET e3 = RBRACKET e4 = IDENTIFIER e5 = SEMICOL
  { SDelete(e4) }
  
  (*For loop:*)
| e1 = FOR e2 = LPAREN e3 = varassign e4 = SEMICOL e5 = expr e6 = SEMICOL e7 =assign e8 = RPAREN e9 = stmt 
  { SScope([e3; SWhile(e5, SScope([e9;e7]))] )  }



stmtlist:
| e1 = stmt e2 = stmtlist
  {e1::e2}
| 
  {[]}



varassign:
| e1 = ty e2 = IDENTIFIER  e3 = EQUAL e4 = expr
     { SVarDef(e1, e2, e4) }
| e = assign
  { e }


assign:
| e1 = IDENTIFIER e2 = LPAREN e3 = exprlist e4 = RPAREN
  { SExpr(ECall(e1, e3)) }

| e1 = IDENTIFIER e2 = EQUAL e3 = expr
  { SVarAssign(e1, e3) }

(*struct array with any type of expression*)
| e1 = IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET e5 = DOT e6 = IDENTIFIER e7 = EQUAL e8 = expr
  { SArrayAssign(e1, e3, Some e6, e8) }
(*regular array with any type of expression*)
| e1 =  IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET e5 = EQUAL e6 = expr
  { SArrayAssign(e1, e3, None, e6) }

(*a1[2].x++;*)
| e1 = IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET e5 = DOT e6 = IDENTIFIER e7 = INCREMENT
  { SArrayAssign(e1, e3, Some e6, EBinOp(Plus, EArrayAccess(e1, e3, Some e6), EInt(1))) }
| e1 = IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET e5 = DOT e6 = IDENTIFIER e7 = DECREMENT
  { SArrayAssign(e1, e3, Some e6, EBinOp(Minus, EArrayAccess(e1, e3, Some e6), EInt(1))) }

(*a[6]++;*)
| e1 =  IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET e5 = INCREMENT
  { SArrayAssign(e1, e3, None, EBinOp(Plus, EArrayAccess(e1, e3, None), EInt(1))) }
| e1 =  IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET e5 = DECREMENT
  { SArrayAssign(e1, e3, None, EBinOp(Minus, EArrayAccess(e1, e3, None), EInt(1))) }

(*increment and decrement:*)
| e1 = IDENTIFIER e2 = INCREMENT  
  { SVarAssign(e1,EBinOp(Plus,EVar(e1), EInt(1))) }
| e1 = IDENTIFIER e2 = DECREMENT 
  { SVarAssign(e1,EBinOp(Minus,EVar(e1), EInt(1))) }




expr:
| e = IDENTIFIER
  { EVar(e) }
| e = UINT
  { EInt(e) }
| e = HEX
  {EInt(e)}

| e1 = SINGLEQUOTE e2 = CHAR e3 = SINGLEQUOTE
  { EChar(e2) }
| e = CHAR
  { EChar(e) }
| e = STRING
  { EString(e) }


(*Binary operations:*)
| e1 = expr e2 = "+" e3 = expr
  { EBinOp(Plus, e1, e3) }
| e1 = expr e2 ="-" e3 = expr  
  { EBinOp(Minus, e1, e3) }
| e1 = expr e2 ="*" e3 = expr  
  { EBinOp(Mult, e1, e3) }
| e1 = expr e2 ="/" e3 = expr  
  { EBinOp(Div, e1, e3) }
| e1 = expr e2 ="%" e3 = expr  
  { EBinOp(Mod, e1, e3) }
| e1 = expr e2 ="<<" e3 = expr  
  { EBinOp(ShiftLeft, e1, e3) }
| e1 = expr e2 =">>" e3 = expr  
  { EBinOp(ShiftRight, e1, e3) }
| e1 = expr e2 =">" e3 = expr  
  { EBinOp(Gt, e1, e3) }
| e1 = expr e2 ="<" e3 = expr  
  { EBinOp(Lt, e1, e3) }
| e1 = expr e2 ="<=" e3 = expr  
  { EBinOp(Le, e1, e3) }
| e1 = expr e2 =">=" e3 = expr  
  { EBinOp(Ge, e1, e3) }
| e1 = expr e2 ="==" e3 = expr  
  { EBinOp(Eq, e1, e3) }
| e1 = expr e2 ="!=" e3 = expr  
  { EBinOp(Ne, e1, e3) }
| e1 = expr e2 ="&" e3 = expr  
  { EBinOp(BAnd, e1, e3) }
| e1 = expr e2 ="|" e3 = expr  
  { EBinOp(BOr, e1, e3) }
| e1 = expr e2 ="&&" e3 = expr  
  { EBinOp(LAnd, e1, e3) }
| e1 = expr e2 ="||" e3 = expr  
  { EBinOp(LOr, e1, e3) }


(*Unary operations:*)
| e1 = "!" e2 = expr
  { EUnOp(LNot, e2) }
| e1 = "~" e2 = expr
  { EUnOp(BNot, e2) }
| e1 = "-" e2 = expr %prec UMINUS
  { EUnOp(UnaryMin, e2) }


| e1 = IDENTIFIER e2 = LPAREN e3 = exprlist e4 = RPAREN
  { ECall(e1, e3) }
| e1 = NEW e2 = ty e3 = LBRACKET e4 = expr e5= RBRACKET
  { ENew (e2, e4) }

| e1 = IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET
  { EArrayAccess(e1, e3, None) }
| e1 = IDENTIFIER e2 = LBRACKET e3 = expr e4 = RBRACKET e5 = DOT e6 = IDENTIFIER
  { EArrayAccess(e1, e3, Some e6) }

| e1 = LPAREN e2 = expr e3 = RPAREN
    { e2 }


exprlist:
|
  {[]}
| e1 = expr
  {[e1]}
| e1 = expr e2 = COMMA e3 = exprlist
  {e1::e3}



unop:
| "!"
  { LNot }
| "~"     
  { BNot }
| "-"
  { UnaryMin }

binop:
| "+"
  { Plus }
| "-"
  { Minus }
| "*"
  { Mult }
| "/"
  { Div }
| "%"
  { Mod }
| "<"
  { Lt }
| ">"
  { Gt }
| "<="
  { Le }
| ">="
  { Ge }
| "=="
  { Eq }
| "!="
  { Ne }
| "&"
  { BAnd } 
| "|"
  { BOr }
| "&&"
  { LAnd }
| "||"
  { LOr }
| "<<"
  { ShiftLeft }
| ">>"
  { ShiftRight }



ty:
| VOID
   { TVoid }
| TYPECHAR
   { TChar }
| TYPEINT
   { TInt } 
| e = IDENTIFIER
  { TIdent(e) }
| e1 = ty e2 = "*"
  { TPoint(e1) } 



params:
| e = paramslist
  { e }

paramslist:
| 
  { []}
| e1 = ty e2 = IDENTIFIER e3 = COMMA e4 = paramslist
  { (e1,e2)::e4 }
| e1 = ty e2 = IDENTIFIER
  { [(e1,e2)] }


structs:
| e = structslist
  { e }

structslist:
| 
  {[]}
| e1 = ty e2 = IDENTIFIER e3 = SEMICOL e4 = structslist
  { (e1,e2)::e4 }


global:
| e1 = ty e2 = IDENTIFIER e3 = LPAREN e4 = params e5 = RPAREN e6 = stmt
  { GFuncDef(e1, e2, e4, e6) }
| e1 = EXTERN e2 = ty e3 = IDENTIFIER e4 = LPAREN e5 = params e6 = RPAREN e7 = SEMICOL
  { GFuncDecl(e2, e3, e5) }
| e1 = ty e2 = IDENTIFIER e3 = "=" e4 = expr  e5 = SEMICOL
  { GVarDef(e1, e2, e4) }
| e1 = EXTERN e2 = ty e3 = IDENTIFIER e4 = SEMICOL
  { GVarDecl(e2, e3) }
 | e1 = STRUCT e2 = IDENTIFIER e3 = LCURBRACKET e4 = structs e5 = RCURBRACKET e6 = SEMICOL
  { GStruct(e2, e4) } 


