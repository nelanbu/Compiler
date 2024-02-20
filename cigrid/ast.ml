(*
uop ::= ! | ~ | - 
bop ::= + | - | * | / | % | < | > | <= | >= | == | != | & | | | && | || | << | >> 
T ::= TVoid | TInt | TChar | TIdent(r) | TPoint(T) 
e ::= EVar(r) | EInt(i) | EChar(c) | EString(r) 
| EBinOp(bop, e, e) | EUnOp(uop, e) | ECall(r, e) 
| ENew(T, e) | EArrayAccess(r, e, rˆ) 
s ::= SExpr(e) | SVarDef(T, r, e) | SVarAssign(r, e) 
| SArrayAssign(r, e, r, e ˆ ) | SScope(s) | SIf(e, s, sˆ) 
| SWhile(e, s) | SBreak | SReturn(ˆe) | SDelete(r) 
g ::= GFuncDef(T, r,(T, r), s) | GFuncDecl(T, r,(T, r)) 
| GVarDef(T, r, e) | GVarDecl(T, r) | GStruct(r,(T, r)) 
p ::= Prog(g) 
*)

type uop =
| LNot  (** Logical Not (!) *)
| BNot  (** Bitwise complement (~) *)
| UnaryMin   (** Unary minus *)

type bop =
| Plus    (** arithmetic + *)
| Minus   (** arithmetic - *)
| Mult     (** * *)
| Div      (** / *)
| Mod      (** % *)
| Lt       (** <  *)
| Gt       (** >  *)
| Le       (** <=  *)
| Ge       (** >=  *)
| Eq       (** ==  *)
| Ne       (** !=  *)
| BAnd     (** bitwise and & *)
| BOr      (** inclusive-or | *)
| LAnd     (** logical and && *)
| LOr      (** logical or || *)
| ShiftLeft  (** shift left << *)
| ShiftRight  (** shift right >> *)



type t =
| TVoid
| TInt
| TChar
| TIdent of string
| TPoint of t

type e =
| EVar of string
| EInt of int
| EChar of char
| EString of string
| EBinOp of bop * e * e
| EUnOp of uop * e
| ECall of string * (e list)
| ENew of t * e
| EArrayAccess of string * e * (string option)

type s =
| SExpr of e
| SVarDef of t * string * e
| SVarAssign of string * e
| SArrayAssign of string * e * (string option) * e
| SScope of (s list)
| SIf of e * s * (s option)
| SWhile of e * s
| SBreak
| SReturn of (e option)
| SDelete of string



type g =
| GFuncDef of t * string * ((t * string) list) * s
| GFuncDecl of t * string * ((t * string) list)
| GVarDef of t * string * e
| GVarDecl of t * string
| GStruct of string * ((t * string) list)


type p =
| Program of (g list)




(*Pretty print for AST: *)


(*print binary operations*)
let print_BOP bop =
  match bop with
  | Plus -> "+"
  | Minus -> "-"  
  | Mult -> "*"   
  | Div -> "/"    
  | Mod -> "%"
  | Lt -> "<"     
  | Gt -> ">"    
  | Le -> "<=" 
  | Ge -> ">="
  | Eq -> "=="
  | Ne -> "!="
  | BAnd -> "&"
  | BOr -> "|"
  | LAnd -> "&&"
  | LOr -> "||"
  | ShiftLeft -> "<<" 
  | ShiftRight -> ">>"
;;

(*print unary operations*)
let print_UOP uop =
  match uop with
  | LNot -> "!"
  | BNot -> "~"
  | UnaryMin -> "-"
;;

(*Print types*)
let rec print_T ty = 
  match ty with
  | TInt -> "TInt"
  | TChar -> "TChar"
  | TVoid -> "TVoid"
  | TIdent(str) -> "TIdent(\"" ^ str ^ "\")"
  | TPoint(typ) -> "TPoint(" ^ (print_T typ) ^ ")"
  ;;


(*Helper function to print t * string lists*)
let rec print_Lst_T_Str l =
  match l with
  | [] -> ""
  | [(ty, ident)] -> "(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\")" 
  | (ty, ident)::xs -> "(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\") " ^ (print_Lst_T_Str xs) ;;

(*Function to pretty print the expressions*)
let rec print_E expr = 
  match expr with
  | EVar(str) -> "EVar(\"" ^ str ^ "\")"
  | EInt(i) -> "EInt(" ^ string_of_int(i) ^ ")"
  | EChar(c) -> "EChar('" ^ ( if c = '\"' then "\\\"" else (Char.escaped c) ) ^ "')"
  | EString(str) -> "EString(\"" ^ str ^ "\")"
  | EBinOp (bop, e1, e2) -> "EBinOp(" ^ (print_BOP bop) ^ ", " ^ (print_E e1) ^ ", " ^ (print_E e2) ^ ")"
  | EUnOp (uop, e) -> "EUnOp(" ^ (print_UOP uop) ^ ", " ^ (print_E e) ^ ")" 

  | ECall(str, e) -> "ECall(\"" ^ str ^ "\", {" ^ (String.concat "\n" (List.map print_E e)) (* (print_Lst_E e) *) ^ "})"

  | ENew (t, e) -> "ENew(" ^ (print_T t) ^ ", " ^ (print_E e) ^ ")"

  | EArrayAccess(str, e, None) -> "EArrayAccess(\"" ^ str ^ "\", " ^ (print_E e) ^ ", )"
  | EArrayAccess(str, e, Some str2) -> "EArrayAccess(\"" ^ str ^ "\", " ^ (print_E e) ^ ", \"" ^ str2 ^ "\")"

(*Helper function to print list of expressions but it is not used because I changed to maping the list to expression print only*)
and print_Lst_E l =
  match l with
  | [] -> ""
  | [e] -> " " ^ (print_E e) 
  | e::xs ->  (print_E e) ^ " " ^ (print_Lst_E xs) ;;


(*Function to pretty print the statements*)
let rec print_S s = 
  match s with
  | SExpr(e) -> "SExpr(" ^ (print_E e) ^ ")"
  | SVarDef(ty, ident, expr) -> "SVarDef(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\", "^ (print_E expr) ^  ")"
  | SVarAssign(str, e) -> "SVarAssign(\"" ^ str ^ "\", " ^ (print_E e) ^ ")"

  | SArrayAssign(str, e1, None, e2) -> "SArrayAssign(\"" ^ str ^ "\", " ^ (print_E e1) ^ ", , " ^ (print_E e2) ^ ")"
  | SArrayAssign(str1, e1, Some str2, e2) -> "SArrayAssign(\"" ^ str1 ^ "\", " ^ (print_E e1) ^ ", \"" ^ str2 ^ "\", " ^ (print_E e2) ^ ")"

  | SScope(lst) -> "\nSScope ({\n" ^ (String.concat "\n" (List.map print_S lst)) ^ "\n})"

  | SIf(e, s, None) -> "SIf(" ^ (print_E e) ^ ", " ^ (print_S s) ^ ", )"
  | SIf(e, s1, Some s2) -> "SIf(" ^ (print_E e) ^ ", " ^ (print_S s1) ^ ", " ^ (print_S s2) ^ ")"

  | SWhile(e, s) -> "SWhile(" ^ (print_E e) ^ ", " ^ (print_S s) ^ ")"
  | SBreak -> "SBreak"
  | SReturn(None) -> "SReturn()"
  | SReturn(Some e) -> "SReturn(" ^ (print_E e) ^ ")"   
  | SDelete(str) -> "SDelete(\"" ^ str ^ "\")"


(*Function to pretty print the globals*)
let print_G g =
    match g with
    | GFuncDef(ty, ident, param_lst, s_lst) -> "GFuncDef(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\", " ^
                                                   "{" ^ (print_Lst_T_Str param_lst) ^ "}, " ^ (print_S s_lst) ^ ")"   
    | GFuncDecl(ty, ident, lst) -> "GFuncDecl(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\", " ^ "{" ^ (print_Lst_T_Str lst) ^ "})"
    | GVarDef(ty, ident, e) -> "GVarDef(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\", " ^ (print_E e) ^ ")"
    | GVarDecl(ty, ident) -> "GVarDecl(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\")"
    | GStruct(ident, lst) -> "GStruct(\"" ^ ident ^ "\", {\n" ^ (print_Lst_T_Str lst) ^ "})"
;;

(*Helper function to pass each global in the global list of the program to the print global function*)
let rec print_G_lst g_lst = 
  match g_lst with 
  | [] -> ""
  | [g] -> (print_G g)
  | g::xs -> (print_G g) ^ "\n" ^ (print_G_lst xs) ;;


(*Pretty print begins here by taking the program and calling the print global function for all globals inside it*)
let print_P p =
  match p with
  | Program(g_lst) -> (print_G_lst g_lst) ;;
 
