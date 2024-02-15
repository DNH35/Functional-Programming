let _ = Printf.printf "Cum!\n"

(*
this is how we write comment   
*)

(* [[1; 2; 3]; [0]]
["String"; "List"] *)

(*
Tuple array   
*)
(* [(1, 2, 3)] *)

let arr = [|1; 2; 3|];;
arr.(1);;

(*
use ref when use let
use := to change content   
^ for concantentaiton string
@ for concatenating list 
*)

let lst1  = [1; 2; 3] ;;
List.hd lst1;;
(*
append 0 to lst1 and make a new list while keeping lst1   
*)
0 :: lst1;; 

let z = 33 in (let w = 55 in z * w);; (*only done locally *)
let f x = 
  let y = x * x in
  x + y;;

let x = 10 ;;
if x > 10 then "bigger" else "smaller";;

let g = fun y z -> 2 * y - z;;
let g y z =  2 * y - z;; (* can declare a function*)
(fun x y -> 2 * x - y) 10 20;;
List.map (fun x -> 2 * x - 3) [1; 2; 3; 4; 5];; (* apply function on the list*)
 
(* recursive function*)
let rec sum_to x = 
  if x = 0 then 0 else x + sum_to (x - 1);;

  (*
     let rec sum_to (x : int) : int = 
  if x = 0 then 0 else x + sum_to (x - 1);;
  to specifiy type
  *)
  let rec sum_to (x : int) : int = 
    match x with
     | 0->0
     | x' -> x' + sum_to (x' - 1);;
  
  let rec list_length lst = 
    match lst with
    | [] -> 0
    | _ :: t -> 1 + list_length t
  ;;

  let rec filter f lst = 
    match lst with 
    | [] -> []
    | h :: t ->
      if f h
        then h :: filter f t
    else filter f t;;

    (* let rec sum_lst lst:
      let rec sum_iter rest sum =
        match rest with
        | [] -> sum
        | h :: t  -> sum_iter t 
        ion know bruh*)

        (*
        Conditionals:
        Use = for comparison
        Use <> for not equal   
        *)
let rec sum_to_square n = 
    if n = 0 then 0 else (n * n) + sum_to_square(n - 1);;

let rec sum_to_square_match n = 
  match n with
  |0 -> 0
  |_ -> n * n + sum_to_square_match(n - 1);;
    