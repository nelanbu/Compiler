(*Yagmur Eren*)


type tree = 
  | Node of string * tree * tree
  | Leaf of int;;

(*PRE_ORDER*)
let pre_order t = 
  let rec pre_acc acc = function
    | Leaf(i) -> (print_string ("Leaf:" ^ (string_of_int i) ^ "\n" ); acc)
    | Node (s, left, right) -> (print_string ("Node:" ^ s ^ "\n");
                                pre_acc (pre_acc acc right) left)
  in pre_acc [] t;;


(*IN_ORDER*)
let in_order t = 
  let rec in_acc acc = function
    | Leaf(i) -> (List.cons ("Leaf:" ^ (string_of_int i) ^ "\n" ) acc)
    | Node (s, left, right) -> (in_acc (List.cons ("Node:" ^ s ^ "\n") (in_acc acc left)) right)
  in in_acc [] t;;


(*POST_ORDER*)
let post_order t = 
  let rec post_acc acc = function
    | Leaf(i) -> (print_string ("Leaf:" ^ (string_of_int i) ^ "\n" ); acc)
    | Node (s, left, right) -> (post_acc(post_acc acc right) left;
                                print_string ("Node:" ^ s ^ "\n") ; acc)
  in post_acc [] t;;


(*LIST*)
let list tree =
  let rec work acc = function
    | Node(s, left, right) -> work (work acc right) left
    | Leaf (i) -> i::acc
  in List.rev (work [] tree);;

let str_list list =
  String.concat "," list;;
          
let comma_sep_list list =
  let listofstr = List.map (fun x -> string_of_int x) list in
  match list with
  | [] -> ""
  | _ -> str_list listofstr;;

open Printf
let pretty list = (*print string list*)
  List.iter (printf "%s") list;;


(*SIZE*)
let rec size = function
  | Node(_, left, right) -> size left + size right + 1
  | Leaf(_) -> 1;;


(*DEPTH*)
let depth t =
  let rec dep m = function (* d records current level, m records max depth so far *)
    | [] -> m
    | (Leaf(_),d)::tl -> dep (max m d) tl
    | (Node (_,l,r),d)::tl -> dep (max m d) ((l,d+1)::(r,d+1)::tl)
  in 
  dep 0 [(t,0)];;


(*GET INPUT*)
let get_command str_input t=

    (*let t = Node("this text", Node("why", Leaf(13), Leaf(4)), Leaf(8)) in  *)

  match str_input with
  | "pre-order" -> print_string (comma_sep_list (pre_order t))
  | "in-order" -> pretty (in_order t)
  | "post-order" -> print_string (comma_sep_list (post_order t))
  | "list" -> print_endline (comma_sep_list (list t))
  | "size" -> print_endline (string_of_int (size t))
  | "depth" -> print_endline (string_of_int (depth t))
  | _ -> exit(1)
  ;;


  let safe_int_of_string s =
    try (int_of_string s) with
    | Failure(_) | Invalid_argument(_) -> exit (1)

let rec form_tree user_input =  (*Each line is string list*)
  if (String.length user_input > 0 ) then 
    let lst = String.split_on_char ':' user_input in 
    let key_word = List.nth lst 0 in
    let value = List.nth lst 1 in
    match key_word with
    | "Leaf" -> begin Leaf(safe_int_of_string value) end
    | "Node" -> begin Node(value, form_tree (read_line()), form_tree (read_line())) end
    | _ -> exit (1)
  else exit(1);;


let main = 
  try
    let tree = form_tree (read_line()) in
    if (Array.length Sys.argv) > 2 then exit (1) 
    else if (Array.length Sys.argv) < 2 then exit (1)
    else (get_command Sys.argv.(1) tree)
  with 
  | End_of_file -> exit(1);;


main ;;
