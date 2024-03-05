(*Part A*)

(*A.1*)
let fibonacci n = 
  if n = 0
    then 0
else 
  let num1 = ref 1 in
  let num2 = ref 1 in
  let i = ref 3 in
    while !i <= n do 
      let sum = !num1 + !num2 in
      num1 := !num2;
      num2 := sum;
      i := !i + 1
    done;
    !num2

let fibonacci2 n = 
  if n = 0
    then 0
else 
  let num1 = ref 1 in
  let num2 = ref 1 in
  for _ = 3 to n do 
    let sum = !num1 + !num2 in
    num1 := !num2;
    num2 := sum;
  done;
  !num2

(*A.2*)

let bubble_sort arr = 
  for _ = 1 to (Array.length arr) - 1 do 
    for j = 0 to (Array.length arr) - 2 do 
      if arr.(j) > arr.(j + 1) 
        then 
          let temp = arr.(j) in 
          arr.(j) <- arr.(j + 1);
          arr.(j + 1) <- temp
  done;
done;
;;

(*Part B*)

(*B.1*)
let meters_per_foot = 0.3048

let foot_per_in = (1. /. 12.)

let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> i *. foot_per_in *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)

let gram_per_kilo = 1000.

let grams_per_slug = 14593.903203

let get_grams mass = 
  match mass with 
  | `Gram gram -> gram
  | `Kilo kilo -> kilo *. gram_per_kilo
  | `Slug slug -> slug *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

let sec_per_min = 60.
let sec_per_hr = 3600.

let hr_per_day = 24.
let get_seconds time = 
  match time with 
  | `Second sec -> sec
  | `Minute min -> min *. sec_per_min
  | `Hour hr -> hr *. sec_per_hr
  | `Day day -> day *. hr_per_day *. sec_per_hr

let time_add a b = `Second (get_seconds a +. get_seconds b)

let unit_add a b = 
  match (a, b) with 
  | (`Length a', `Length b') -> `Length (length_add a' b')
  | (`Mass a', `Mass b') -> `Mass (mass_add a' b')
  | (`Time a', `Time b') -> `Time (time_add a' b')
  | (_, _) -> failwith ("Not compatible type")

(*We won't encouter a combinatorical explosion because each of each of the unit class
is independent of each other since each of them has its own conversion and addition. *)

(*Part C*)

(*C.1*)

let rec make_gram g =
  let get_grams = g
  and get_slugs = g /. grams_per_slug
  and unit_type = `Gram
  and compatible other =
    match other#unit_type with
    | `Gram | `Slug -> true
    | _ -> false (* internal definitions *)
  in
    object
      method get_grams = get_grams
      method get_slugs = get_slugs
      method unit_type = unit_type
      method compatible other = compatible other
      method add other = 
        if compatible other 
          then
        match other#unit_type with 
          | `Gram -> make_gram (get_grams +. other#get_grams)
          (* | `Slug -> make_gram (get_grams +. other#get_slugs *. grams_per_slug) *)
          | _ -> failwith("Incompatibile type")
      else failwith("Incompatibile type")
    end

(*C.2*)

(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
  object
    method value = i
    method show = string_of_int i
    method is_zero = i = 0
    method is_number = true
    method evaluate _ _ = make_number i  (* must evaluate to an object *)
    method derive _ = make_number 0  (* derivative of a number is 0 *)
  end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
  object
    method value = failwith "variable has no numerical value"
    method show  = v
    method is_zero = false
    method is_number = false
    method evaluate v' n =
      if v = v'
        then make_number n
        else make_variable v
    method derive v' =
      if v = v'
        then make_number 1  (* d/dx(x) = 1 *)
        else make_number 0  (* d/dx(y) = 0 *)
  end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
    | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
          make_number (expr1#value + expr2#value)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_sum (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (expr1#derive v) (expr2#derive v)
          end

(* Evaluate a message-passing expression with a number
   substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

let rec make_product expr1 expr2 = 
  match () with
    | _ when expr1#is_zero -> make_number 0  (* 0 * expr = 0 *)
    | _ when expr2#is_zero -> make_number 0  (* expr * 0 = 0 *)
    | _ when expr1#is_number && expr1#value = 1 -> expr2
    | _ when expr2#is_number && expr2#value = 1 -> expr1
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
          make_number (expr1#value * expr2#value)
    | _ ->  
          object
            method value = failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_product (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (make_product (expr1#derive v) expr2) (make_product expr1 (expr2#derive v))
          end


