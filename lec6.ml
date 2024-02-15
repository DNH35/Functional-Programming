(*
let x = 1 and y = x in x + y
will give ubound value x because y = x doesn't refer to x = 1

Desugar:
(fun x y -> x + y) 1 x

let x = 1 in
let y = 1 in
  x + y
*)

let x = 1 in
let y = x in
  x + y

(*
(fun x -> x + x) 10
(fun x y -> x + y) 10 20  
(fun x y -> x + y) 10 (x * x) [unbound]

let x = 10 in
let y = x * x in
x + y
(fun x -> let y = x * x in x + y) 10
(fun x -> (fun y -> x + y) (x * x)) 10

(*
To print:
let f x = let _ = Printf.printf "x = %d\n" x in x;;   
*)
  

*)
(* let make_addn n = fun x -> x + n *)
(*
(fun n -> (fun n -> n + n)) 3 wrong

Lambda shielding:
Cannot substitute the argument x from the x body
Have to use different inner binding variable
*)

let multi_stage_add = 
  fun a -> 
    fun b ->
      fun c ->
        a + b + c

let make_addn = (+)
let add42 = make_addn 42