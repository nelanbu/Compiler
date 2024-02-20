(*FUNCTIONS TO CHECK A NUMBER IS PRIME, TO FORM A LIST OF PRIME NUMBERS AND PRIMES FUNCTION
WHICH DECIDES HOW MANY PRIME NUMBER WE WANT*)

let is_prime n =
  let rec check_prime x y =
    match y with
    | 1 -> true
    | _ -> (x mod y <> 0) && check_prime x (y-1) 
  in match n with
  | 0 | 1 -> false
  | _ -> check_prime n (n-1);;
  
  
let rec prime_find p x =
  match p with 
  | 0 -> []
  | _ ->
      let primes_list = if is_prime x then prime_find (p-1) (x+1) else prime_find p (x+1) in
      if is_prime x then x::primes_list else primes_list;;
  
      
let primes n = 
  match n with 
  | _ when n < 101 && n > 0 -> prime_find n 2
  | _ -> exit(1) ;;     


(*PRINT NICELY FUNCTIONS*)

let str_list list =
  String.concat "," list;;
          
let pretty list =
  let listofstr = List.map (fun x -> string_of_int x) list in
  match list with
  | [] -> ""
  | _ -> str_list listofstr;;



(*PRINT NICELY FUNCTIONS*)

let str_list list =
  String.concat "," list;;
          
let pretty list =
  let listofstr = List.map (fun x -> string_of_int x) list in
  match list with
  | [] -> ""
  | _ -> str_list listofstr;;



(*GET USER INPUT AS ARGUMENT TO PRIMES*)




let main =
  let user_input = int_of_string(Sys.argv.(1)) in
  
  if Array.length Sys.argv > 2 then 
    exit(2) 
  else if Array.length Sys.argv < 2 then (
    try Some (int_of_string(Sys.argv.(1))) with
    | Failure(_) -> exit(2) )
  else  
    (print_string (pretty (primes user_input));
     exit (0)
    );;

main;;