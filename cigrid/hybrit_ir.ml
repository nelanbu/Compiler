open Ast

type param = t * string

type ir_stmt =
| IRSExpr of e
| IRSVarAssign of string * e
| IRSVarDecl of string * t

type ir_blockend =
 | IRSReturn of e option
 | IRSBranch of e * string * string
 | IRSJump of string 

type ir_block = IRBlock of string * (ir_stmt list * ir_blockend)

type ir_global = IRFunc of string  * (t * param list * ir_block list)



 (*Pretty print for Hybrid IR: *)

let rec print_params p =
  match p with
  | [] -> ""
  | [(ty, ident)] -> "(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\")" 
  | (ty, ident)::xs -> "(" ^ (print_T ty) ^ ", \"" ^ ident ^ "\") " ^ (print_params xs) ;;

  
let print_IR_blockend be =
  match be with
    | IRSReturn(None)-> "IRSReturn"
    | IRSReturn(Some e) -> "IRSReturn(" ^ (print_E e) ^ ")"
    | IRSBranch(e, str1, str2)-> "IRSBranch(" ^ (print_E e) ^ ", \"" ^ str1 ^ "\", \"" ^ str2 ^ "\")"
    | IRSJump(str)-> "IRSJump(" ^ str ^ ")";;

    
let print_IR_stmt s =
  match s with
  | IRSExpr(e) -> "IRSExpr(" ^ (print_E e) ^ ")"
  | IRSVarAssign(str, e)-> "IRSVarAssign(\"" ^ str ^ "\", " ^ (print_E e) ^ ")"
  | IRSVarDecl(str, ty) -> "IRSVarDecl(\"" ^ str ^ "\", " ^ (print_T ty) ^ ")";;


let print_IR_block b =
  match b with
    | IRBlock(str, (lst_irstmt, ir_be)) -> "IRBlock({" ^ str ^ ", \n" ^ (String.concat "\n" (List.map print_IR_stmt lst_irstmt)) ^ " \n" ^ 
                                        (print_IR_blockend ir_be) ^ "})";;


(*Function to pretty print the globals*)
let print_IR_global g = 
  match g with
  | IRFunc(str, (ty, lst_param, lst_irblck)) -> "IRFunc(" ^ str ^ ", " ^ (print_T ty) ^ ", {" ^ (print_params lst_param) ^ "}, \n" ^ 
  (String.concat "\n" (List.map print_IR_block lst_irblck)) ^ ")";;

let rec print_IR_G_lst g_lst = 
    match g_lst with 
    | [] -> ""
    | [g] -> (print_IR_global g)
    | g::xs -> (print_IR_global g) ^ "\n" ^ (print_IR_G_lst xs) ;;
  





(*Convert functions from ast to hybrit ir*)
let rec convert_param lst =
  match lst with
  (* | [] ->  *)
  | [(ty, str)] ->  IRSVarDecl(str, ty)
  | (ty, str)::xs ->  IRSVarDecl(str, ty); (convert_param xs);;


let rec convert_stmt stmt_lst =  
  match stmt_lst with
  | [] -> []
  | stmt::xs -> match stmt with
          | SExpr(e) -> IRSExpr(e)::(convert_stmt xs)
          | SVarDef(ty, str, e)-> IRSVarDecl(str, ty) :: IRSVarAssign(str, e)::(convert_stmt xs)
          | SVarAssign(str, e)-> IRSVarAssign(str, e)::(convert_stmt xs)
          | _ -> exit 1;;


let convert_blck_end be =
  match be with
  | SReturn(None) -> IRSReturn(None)
  | SReturn(Some e) -> IRSReturn(Some e)
;;


  let get_last_element lst =
    List.hd (List.rev lst);;
  
  let delete_last_element lst =
    List.rev (List.tl (List.rev lst));; 


let rec convert_scope ident sp =
  match sp with
  | SScope(stmt_lst) -> [IRBlock(ident, (( convert_stmt (delete_last_element stmt_lst) ), 
                                                                    (convert_blck_end (get_last_element stmt_lst ))))]
;;


let rec convert_global g_lst =
    match g_lst with
    | GFuncDef(ty, ident, param_lst, blck_lst)::rest -> IRFunc(ident, (ty, param_lst, convert_scope ident blck_lst))
                                                        ::(convert_global rest)
    | _ -> []                                                                 
   ;;


let convert_program p = 
  match p with
  | Program(g_lst) -> (convert_global g_lst) ;;

