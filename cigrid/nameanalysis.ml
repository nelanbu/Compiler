
open Ast


(*Name analysis*)
let name_analysis p =
  let check_global g =
    let rec stmt_checkList variable_list stmtList =  (*check statements whether they are in the list*)
      let rec expr_checkList variable_list expr =  (*check expressions whether they are in the list*)
        match expr with
        | EVar(ident) -> if not (List.mem ident variable_list) then exit 2; (*if particular identifier is not in the variable list exit 2*)
        | EBinOp(str, e1, e2) -> expr_checkList variable_list e1; expr_checkList variable_list e2;
        | EUnOp(str, e) -> expr_checkList variable_list e;
        | _ -> ()
      in
      match stmtList with
      | stmt::stmtListrem -> (
        match stmt with 
        |SVarDef(ty, ident, e) -> expr_checkList variable_list e; 
                                  stmt_checkList (ident::variable_list) stmtListrem;
        |SVarAssign(ident, e) ->  if not (List.mem ident variable_list) then exit 2; 
                                  expr_checkList variable_list e;
                                  stmt_checkList variable_list stmtListrem;
        |SExpr(e) ->              expr_checkList variable_list e;
                                  stmt_checkList variable_list stmtListrem;
        |SWhile(e, s) ->          expr_checkList variable_list e;
                                  stmt_checkList variable_list (s::stmtListrem);
        | SIf(e, s, None) ->      expr_checkList variable_list e;
                                  stmt_checkList variable_list (s::stmtListrem);
        | SIf(e1, s1, Some s2) -> expr_checkList variable_list e1;
                                  stmt_checkList variable_list (s1::stmtListrem);
                                  stmt_checkList variable_list (s2::stmtListrem);
        (* | SReturn(None) -> *)
        | SReturn(Some e) ->      expr_checkList variable_list e;
        | SScope(s) ->            stmt_checkList variable_list (List.append s stmtListrem);
        |_ -> ();
        )
      | _ -> ()
    in
    match g with
    | GFuncDef(ty, iden, lst, SScope(stmtList)) -> 
      let rec get_varList lst = match lst with 
      | (t, ident)::xl -> ident::(get_varList xl)
      | [] -> []
      in let variable_list = get_varList lst in
      stmt_checkList variable_list stmtList;
    | _ -> ();
  in
  match p with Program(gs) -> List.iter check_global gs;;
