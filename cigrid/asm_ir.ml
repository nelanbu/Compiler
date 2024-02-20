open Ast
open Hybrit_ir

type unop = Inc | Dec | Push | Pop | IMul | IDiv | Not | Neg |
 Setg | Setl | Setge | Setle | Sete | Setne

type binop = Add | Sub | Cmp | Mov | And | Or | Xor 

type bitsize =
 | Byte | Word | DWord | QWord

type displacement = int

type scale = int

type reg = int * bitsize

type op =
 | Imm of int
 | Reg of reg
 | TReg of reg * string
 | Mem of bitsize * reg * reg option * scale * displacement
 | NoOp

type inst =
 | UnOp of unop * op
 | BinOp of binop * op * op
 | Call of string
 | Cqo

 type jbinop = Jl | Jg | Jle | Jge | Je | Jne

 type blockend =
 | Ret
 | Jmp of string
 | JBinOp of jbinop * string * string

 type block = Block of string * (inst list * blockend)

 type func = Func of string * block list



 (*Pretty print for ASM-IR*)

(*print binary operations*)
let print_binary bop =
  match bop with
  | Add -> "\tAdd\t"
  | Sub -> "\tSub\t"
  | Cmp -> "\tCmp\t"
  | Mov -> "\tMov\t"
  | And -> "\tAnd\t"
  | Or -> "\tOr\t"
  | Xor -> "\tXor\t"


(*print unary operations*)
let print_unary uop =
  match uop with
  | Inc -> "\tInc\t"
  | Dec -> "\tDec\t"
  | Push -> "\tPush\t"
  | Pop -> "\tPop\t"
  | IMul -> "\tIMul\t"
  | IDiv -> "\tIDiv\t"
  | Not -> "\tNot\t"
  | Neg -> "\tNeg\t"
  | Setg -> "\tSetg\t"
  | Setl -> "\tSetl\t"
  | Setge -> "\tSetge\t"
  | Setle -> "\tSetle\t"
  | Sete -> "\tSete\t"
  | Setne -> "\tSetne\t"


let print_bitsize bs =
  match bs with
 | Byte ->  "byte"
 | Word -> "word"
 | DWord -> "dword"
 | QWord -> "qword"
 

let print_displacement d = 
match d with
| 0 -> ""
| _ -> " + " ^ string_of_int(d)


let print_reg r =
  match r with
  | (-1, QWord) -> "rsp"
  | (-2, QWord) -> "rax"
  | (-3, QWord) -> "rdx"
  | (num, btsize) -> (* (print_bitsize btsize) ^ *) "r" ^ string_of_int(num)


let print_op op =
  match op with
 | Imm(num) -> string_of_int(num)
 | Reg(n, btsize) -> (print_reg (n, btsize))
 | TReg((n, btsize), str) -> str ^ "_" ^ string_of_int(n)
 | Mem (btsize, r1, None, sc, dsp) -> (print_bitsize btsize) ^ " [" ^ (print_reg r1) ^ (print_displacement dsp) ^ "]"
 | Mem (btsize, r1, Some r2, sc, dsp) -> (print_bitsize btsize) ^ " " ^ (print_reg r1) ^ " + " ^ (print_reg r2) ^ " * " ^
                                          string_of_int(sc) ^ (print_displacement dsp)
 | NoOp -> ""


let print_inst inst =
  match inst with
 | UnOp(uo, o) -> (print_unary uo) ^ "\t" ^ (print_op o)
 | BinOp(bo, o1, o2) -> (print_binary bo) ^ "\t" ^ (print_op o1) ^ ", " ^ (print_op o2)
 | Call(str) -> "call\t" ^ str
 | Cqo -> ""


 let print_jbinop jb =
  match jb with
  | Jl -> "\tJl\t"
  | Jg -> "\tJg\t"
  | Jle -> "\tJle\t"
  | Jge -> "\tJge\t"
  | Je -> "\tJe\t"
  | Jne -> "\tJne\t"
 

 let print_blockend be =
  match be with
 | Ret -> "\n\tret"
 | Jmp(str)-> "\tjmp\t" ^ str 
 | JBinOp(jb, str1, str2)-> (print_jbinop jb) ^ "\t" ^ str1 ^ ", " ^ str2
 

let print_block b = 
  match b with
 | Block(str, (lst_inst, be)) -> str ^ ":\n" ^ (String.concat "\n" (List.map print_inst lst_inst))  ^ (print_blockend be)


 let rec print_func f_lst =  
  match f_lst with
  | [] -> []
  | f::xs -> match f with
          | Func(str, lst_blck) -> ("\tglobal\t" ^ str ^ "\n\tsection  .text\n" ^ (String.concat "\n" (List.map print_block lst_blck)))
                                   ::(print_func xs)
          | _ -> exit 1




(*instruction selection: *)
let bitsize_of_type ty =
  match ty with
  | TInt -> QWord
  | TChar -> QWord


let tmp_reg n = TReg((n, QWord), ".temp")

let change_bitsize oper bitsize =
  match oper with
  | Reg(n, btsize_prev) -> Reg(n, bitsize)
  | TReg((n, bitsize_prev), st) -> TReg((n, bitsize), st) 
  | Mem(bitsize_prev,x, None, y, z) -> Mem(bitsize, x, None, y, z)
  | op -> op 
  


let make_reg l env =
  let (n, ty) = List.assoc l env in
  TReg((n, bitsize_of_type ty), l)


let rec inst_select_expr env n acc reg = function
  (* Variable *)
  | EVar(x) -> (BinOp(Mov, reg, make_reg x env)::acc, n)
  (* Integer constant *)
  | EInt(v) -> (BinOp(Mov, reg, Imm(v))::acc, n)
  | EChar(c) -> (BinOp(Mov, reg, Imm(Char.code(c)))::acc, n)

  (* BinOp: x = e1 + e2 *)
  | EBinOp(Plus, e1, e2) ->
                         let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
                         let n1 = n + 2 in
                         let acc1 = BinOp(Mov, reg, r1)::BinOp(Add, reg, r2)::acc in
                         let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                                         inst_select_expr env n2 acc2 r1 e1
  | EBinOp(Minus, e1, e2) ->
                        let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
                        let n1 = n + 2 in
                        let acc1 = BinOp(Mov, reg, r1)::BinOp(Sub, reg, r2)::acc in
                        let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                                        inst_select_expr env n2 acc2 r1 e1
  | EBinOp(Div, e1, e2) ->
                        let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
                        let (rdx, rax) = (Reg(-3, QWord), Reg(-2, QWord)) in
                        let n1 = n + 2 in
                        let acc1 = BinOp(Mov, rdx, Imm(0))::BinOp(Mov, rax, r1)::UnOp(IDiv, r2)::BinOp(Mov, reg, rax)::acc in
                        let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                                        inst_select_expr env n2 acc2 r1 e1
  | EBinOp(Mod, e1, e2) ->
                        let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
                        let (rdx, rax) = (Reg(-3, QWord), Reg(-2, QWord)) in
                        let n1 = n + 2 in
                        let acc1 = BinOp(Mov, rdx, Imm(0))::BinOp(Mov, rax, r1)::UnOp(IDiv, r2)::BinOp(Mov, reg, rdx)::acc in
                        let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                                        inst_select_expr env n2 acc2 r1 e1

  | EBinOp(Mult, e1, e2) ->
                        let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
                        let (rdx, rax) = (Reg(-3, QWord), Reg(-2, QWord)) in
                        let n1 = n + 2 in
                        let acc1 = BinOp(Mov, rax, r1)::UnOp(IMul, r2)::BinOp(Mov, reg, rax)::acc in
                        let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                                        inst_select_expr env n2 acc2 r1 e1
  | EUnOp(UnaryMin, e) ->
                        let acc1 = UnOp(Neg, reg)::acc in
                        inst_select_expr env n acc1 reg e
  | EBinOp(Eq, e1, e2) ->
                        let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
                        let n1 = n + 2 in
                        let acc1 = BinOp(Cmp, r1, r2)::UnOp(Sete, (change_bitsize reg Byte))::acc in
                        let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                                        inst_select_expr env n2 acc2 r1 e1
  (*less than*)                                      
  | EBinOp(Lt, e1, e2) ->
    let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
    let n1 = n + 2 in
    let acc1 = BinOp(Cmp, r1, r2)::UnOp(Setl, (change_bitsize reg Byte))::acc in
    let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                    inst_select_expr env n2 acc2 r1 e1
 (*greater than*)
  | EBinOp(Gt, e1, e2) ->
    let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
    let n1 = n + 2 in
    let acc1 = BinOp(Cmp, r1, r2)::UnOp(Setg, (change_bitsize reg Byte))::acc in
    let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                    inst_select_expr env n2 acc2 r1 e1
  (*less than or equeal to*)
  | EBinOp(Le, e1, e2) ->
    let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
    let n1 = n + 2 in
    let acc1 = BinOp(Cmp, r1, r2)::UnOp(Setle, (change_bitsize reg Byte))::acc in
    let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                    inst_select_expr env n2 acc2 r1 e1
  (*greater than or equeal*)
  | EBinOp(Ge, e1, e2) ->
    let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
    let n1 = n + 2 in
    let acc1 = BinOp(Cmp, r1, r2)::UnOp(Setge, (change_bitsize reg Byte))::acc in
    let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                    inst_select_expr env n2 acc2 r1 e1
    (*not equal*)
  | EBinOp(Ne, e1, e2) ->
    let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
    let n1 = n + 2 in
    let acc1 = BinOp(Cmp, r1, r2)::UnOp(Setne, (change_bitsize reg Byte))::acc in
    let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
                    inst_select_expr env n2 acc2 r1 e1



 (* | _ -> failwith "TODO" *)


let rec inst_select_ir_stmts env n acc = function
| IRSVarAssign(x, expr)::xs -> let (expr_acc, n2) = inst_select_expr env n [] (make_reg x env) expr in
                              inst_select_ir_stmts env n2 (List.rev_append expr_acc acc) xs
| IRSVarDecl(x,ty)::xs ->     inst_select_ir_stmts ((x,(n,ty))::env) (n+1) acc xs
| [] -> (env, n, List.rev acc)
;;


let inst_select_ir_blockend env n = function
  | IRSReturn(None) ->  ([], Ret, n)
  | IRSReturn(Some e) ->  let (stmt_lst, n1) = inst_select_expr env n [] (Reg(-2, QWord)) e in (stmt_lst, Ret, n1)
  | _ -> exit 1;;

let rec inst_select_ir_block env n = function
  | IRBlock(str, (stmtlst, bend))::rest -> 
        let (env2, n2, instlst) = inst_select_ir_stmts env n [] stmtlst in  
        let (stmt, blck_end, n3) = (inst_select_ir_blockend env2 n2 bend) in
        Block(str, ((List.concat [[(BinOp(Sub, Reg((-1, QWord)), Imm(n3*8)))]; instlst; stmt; [(BinOp(Add, Reg((-1, QWord)), Imm(n3*8)))]]), 
        blck_end))::(inst_select_ir_block env2 n3 rest);
  | [] -> []
 
let rec inst_select_ir_globals n = function 
| IRFunc(str, (ty, lst_param, lst_irblck))::rest ->
  let rec get_env lst_param n env_acc = match lst_param with
                                | (ty, ident)::xs -> get_env xs (n+1) ((ident, (n, ty))::env_acc)
                                | [] -> (n, env_acc) in
                                let (n2, env) = get_env lst_param n [] in
                                let block_list = inst_select_ir_block env n lst_irblck in 
                                Func(str, block_list)::(inst_select_ir_globals n2 rest )
  | [] -> []

(***************************)




(*after spiling*)

let rec spill_reg r =
  match r with 
  | TReg((n, btsize), str) -> Mem(btsize, (-1, QWord), None, 0, n*8)
  | op -> op


let rec spill_stmt n stmt =
  match stmt with
   | BinOp(binop, TReg((n1, btsize1), str1), TReg((n2, btsize2), str2))::xs -> 
                                  BinOp(Mov, Reg((10, QWord)), spill_reg (TReg((n2, btsize2), str2)))
                                  ::BinOp(binop, spill_reg (TReg((n1, btsize1), str1)), Reg((10, QWord)))::(spill_stmt n xs)

   | BinOp(binop, op1, op2)::xs -> BinOp(binop, (spill_reg op1), (spill_reg op2))::(spill_stmt n xs)
   | UnOp(unop, op1)::xs -> UnOp(unop, (spill_reg op1))::(spill_stmt n xs)
   | [] -> []


let rec spill_2 n bl_lst =
  match bl_lst with
  | Block(str, (stmtlst, bend))::rest -> Block(str, (spill_stmt n stmtlst, bend)):: (spill_2 n rest)
  | [] -> []


let spill ir =
  let rec spill_ n ir =
    match ir with
    | Func(str, block_list)::rest -> Func(str, spill_2 n block_list)::(spill_ n rest)
    | [] -> []
  in spill_ 0 ir
