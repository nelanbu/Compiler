(*YAGMUR EREN*)
open Printf
open Ast
open Nameanalysis
open Hybrit_ir
open Asm_ir

(********************************************************************************************************************************)

(*Main program*)
 let main = 
  (*Flags array showing whether flags are added in command line and consisting of bools, 
                    if a command is given by the user it appears as true in the array*)
  (*[| --pretty=print; --line-error; --name-analysis; --asm; --ir, --compile-asm ||]*)
  let flags = [|false; false; false; false; false; false|] in 

  let rec checkFlags i=
    let checkFlag (s:string) : unit=
      match s with
      | "--pretty-print"  ->   flags.(0)   <- true; 
      | "--line-error"    ->   flags.(1)   <- true;
      | "--name-analysis" ->  flags.(2)  <- true;
      | "--asm" ->  flags.(3)  <- true;
      | "--ir" ->  flags.(4)  <- true;
      | "--compile-asm" ->  flags.(5)  <- true;
      | _ -> exit 1; 
    in
    if (i > 0 && i < (Array.length Sys.argv) - 1) then checkFlag Sys.argv.(i);
    if (i < (Array.length Sys.argv) - 2 ) then checkFlags (i+1);
  in

  checkFlags 1;
(*Get file name and read the file*)
  let file = Sys.argv.((Array.length Sys.argv) - 1) in
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
    
  let  result = 
    try Parser.main Lexer.token lexbuf 
    with 
    | Lexer.Error c ->
      if flags.(1) then (*if line error command has been given than the line number is printed*)
      fprintf stderr "%d\n" lexbuf.lex_curr_p.pos_lnum;
      exit 1
    | Parser.Error ->
        if flags.(1) then
        fprintf stderr "%d\n" lexbuf.lex_curr_p.pos_lnum;
        exit 1
  in 

if flags.(0) then print_endline ( print_P result); (*if pretty print command has been given then print the program*)
if flags.(2) then (name_analysis result); (*if name analysis command has been given then call name analysis function*)


if flags.(3) then 
  ( print_endline (String.concat "" ( print_func (spill (inst_select_ir_globals 0 (convert_program result))))); 
);


if flags.(4) then
  (print_endline (print_IR_G_lst (convert_program result)) );

if flags.(5) then
  (
     let oc = open_out "a.asm" in
    Printf.fprintf oc "%s" (String.concat "" ( print_func (spill (inst_select_ir_globals 0 (convert_program result)))));
    flush oc;
    close_out oc;
  )

