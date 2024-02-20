
open Printf
(*To run : ocamlc str.cma calc.ml
          ./a.out*)

type token =
| TokNum of int
| TokAdd 
| TokSub
| TokMul
| TokDiv
| TokLParen
| TokRParen
| TokWhiteSpace
| TokEOF;;

type expr = 
| ExprAdd of expr * expr
| ExprSub of expr * expr     
| ExprMul of expr * expr
| ExprDiv of expr * expr
| ExprNum of int;;


let rec expr_result expr =
  match expr with 
  | ExprAdd(a,b) -> (expr_result a) + (expr_result b)
  | ExprSub(a,b) -> (expr_result a) - (expr_result b)
  | ExprMul(a,b) -> (expr_result a) * (expr_result b)
  | ExprNum(i) -> i
  | ExprDiv(a,b) -> try ((expr_result a) / (expr_result b)) with
                    | Division_by_zero  -> exit (2)
  




(*regular expressions:*)
let re_num = Str.regexp "[0-9]+";;
let re_add = Str.regexp "+";;
let re_sub = Str.regexp "-";;
let re_mul = Str.regexp "*";;
let re_div = Str.regexp "/";;
let re_LeftParen = Str.regexp "(";;
let re_RightParen = Str.regexp ")";;
let re_whitespace = Str.regexp "[ \t\n]+";;


exception IllegalExpression of string;;


(*Lexer:*)
(*
let tokenizer str =
  let rec tok s pos =
    if pos >= String.length s then [TokEOF]
    else
      if(Str.string_match re_num s pos) then 
        let token = Str.matched_string s in
        TokNum (int_of_string (Char.escaped token.[0]))::(tok s (pos+1))

      else if(Str.string_match re_add s pos) then 
        TokAdd :: (tok s (pos+1)) 

      else if(Str.string_match re_sub s pos) then 
        TokSub :: (tok s (pos+1)) 

      else if(Str.string_match re_mul s pos) then 
        TokMul :: (tok s (pos+1))

      else if(Str.string_match re_div s pos) then 
        TokDiv :: (tok s (pos+1)) 

      else if(Str.string_match re_LeftParen s pos) then 
        TokLParen :: (tok s (pos+1)) 

      else if(Str.string_match re_RightParen s pos) then 
        TokRParen :: (tok s (pos+1)) 
      
      else 
        (* raise (IllegalExpression "LexError"); *) exit (1)

  in tok str 0 ;; 
*)


  let tokenizer str =
    let rec tok s pos =
      (*printf "s = %s pos = %d\n" s pos; *)
      if pos >= String.length s then [TokEOF]
      else
        if(Str.string_match re_num s pos) then
          let rec get_num number n = 
            let digit = int_of_string (Char.escaped (Str.matched_string s).[0]) in
            if(Str.string_match re_num s (pos + n)) then
              get_num ((number+digit)*10) (n+1)
            else
              (number+digit, n)
          in
          let (num_value, num_length) = get_num 0 1 in
          TokNum (num_value)::(tok s (pos+num_length))
        else if(Str.string_match re_add s pos) then 
          TokAdd :: (tok s (pos+1)) 
        else if(Str.string_match re_sub s pos) then 
          TokSub :: (tok s (pos+1)) 
        else if(Str.string_match re_mul s pos) then 
          TokMul :: (tok s (pos+1))
        else if(Str.string_match re_div s pos) then 
          TokDiv :: (tok s (pos+1)) 
        else if(Str.string_match re_LeftParen s pos) then 
          TokLParen :: (tok s (pos+1)) 
        else if(Str.string_match re_RightParen s pos) then 
          TokRParen :: (tok s (pos+1)) 
        else if(Str.string_match re_whitespace s pos) then 
          (* TokWhiteSpace :: *)(tok s (pos+1))
        else 
          exit (1)
          (* raise (IllegalExpression "tokenizer") *)
  
    in tok str 0 ;; 


let token_list = ref [];;

let get_token () =
  match !token_list with
  | [] -> (* raise (IllegalExpression "ParseError"); *) exit (0)
  | h::t-> token_list := t; h;;  


(*Parser:*)

(*
  Expr -> term Expr'
  Expr' -> '+' term Expr'
         | '-' term Expr'
         | 
  term -> factor term'
  term' -> '*' factor term'
        | '/' factor term'
        |
  factor -> Num
         | '(' expr ')'   
*)


let rec parseExpr tok =
  let (tok2, expr) = parseTerm tok in
  parseExprPrime tok2 expr


and parseExprPrime tok expr =
  match tok with 
  | TokAdd -> let next = get_token () in
              let (tok2, expr2) = parseTerm next in
              parseExprPrime tok2 (ExprAdd(expr,expr2))
  | TokSub -> let next = get_token () in
              let (tok2, expr2) = parseTerm next in
              parseExprPrime tok2 (ExprSub(expr,expr2))
  | _ -> (tok, expr)


and parseTerm tok =
  let (tok2, expr) = parseFactor tok in
  parseTermPrime tok2 expr


and parseTermPrime tok expr  =
  match tok with
  | TokMul -> let next = get_token () in
              let (tok2, expr2) = parseFactor next in
              parseTermPrime tok2 (ExprMul(expr, expr2))
  | TokDiv -> let next = get_token () in
              let (tok2, expr2) = parseFactor next in
              parseTermPrime tok2 (ExprDiv(expr, expr2))
  | _ -> (tok, expr)


and parseFactor tok = 
  match tok with 
  | TokNum n -> let next = get_token () in (next, ExprNum(n))
  | TokLParen -> let next = get_token () in 
                  let (tok2, expr) = parseExpr(next) in
                  if tok2 == TokRParen then 
                    let next2 = get_token() in
                    (next2, expr)
                  else
                    (* raise (IllegalExpression "ParseError"); *) exit (1) 
  (*| TokWhiteSpace -> exit(0) *)
  | _ -> (* raise (IllegalExpression "ParseError"); *) exit (1)



let rec pretty_print expr =
  match expr with
  | ExprNum(n)    -> printf "%d" n
  | ExprAdd(a, b) -> printf "("; pretty_print a; printf "+"; pretty_print b; printf ")"
  | ExprSub(a, b) -> printf "("; pretty_print a; printf "-"; pretty_print b; printf ")"
  | ExprMul(a, b) -> printf "("; pretty_print a; printf "*"; pretty_print b; printf ")"
  | ExprDiv(a, b) -> printf "("; pretty_print a; printf "/"; pretty_print b; printf ")"


let parseMain user_input=
      let tokenized_input = tokenizer (user_input) in
      (*printf "Length of tokenized list: %d\n" (List.length tokenized_input);*)
      token_list :=tokenized_input; 
      let next = get_token () in
      if next == TokEOF then exit (0)
      else begin
        let (tok, expr) = parseExpr next in
        if tok <> TokEOF then
          begin
          exit(1)
          end
        else 
          expr
      end


let get_user_input () =
  let rec get_input () =
    try 
      let parse_tree = (parseMain (read_line ())) in
      pretty_print parse_tree;
      printf "\n= %d\n" (expr_result parse_tree);
      get_input ()
  with 
  | End_of_file -> exit (0) 
  in
    get_input ();;


let main =
  get_user_input ();;