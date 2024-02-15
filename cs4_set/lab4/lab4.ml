(*Part A*)

(*A.1*)
type point = {x : float; y : float}

type segment = {startp: point; endp : point}

let midpoint_segment (seg : segment) =
  let midpoint_x = abs_float (seg.endp.x -. seg.startp.x) /. 2. in
  let midpoint_y = abs_float (seg.endp.y -. seg.startp.y) /. 2. in
  {x = midpoint_x ; y = midpoint_y}

let segment_length (seg : segment) = 
  sqrt ((seg.endp.x -. seg.startp.x) *. (seg.endp.x -. seg.startp.x) +. 
  (seg.endp.y -. seg.startp.y) *. (seg.endp.y -. seg.startp.y))

let print_point (p : point) = 
  Printf.printf "(%g, %g)\n" p.x p.y

let make_point x_coor y_coor = 
  {x = x_coor; y = y_coor}

let get_coords (p : point) = 
  (p.x, p.y)

let make_segment (p1 : point) (p2 : point) = 
  {startp = p1 ; endp = p2}

let get_points (seg : segment) = 
  (seg.startp, seg.endp)

(*A.2*)
type rectangle = {lower_left : point ; upper_right : point}
type rectangle2 = {lower_x : float; upper_x : float; lower_y : float; upper_y : float}

let make_rectangle ll ur = 
  {lower_left = ll ; upper_right = ur}

let rectangle_lower_segment (rect : rectangle) = 
  make_segment rect.lower_left (make_point rect.upper_right.x rect.lower_left.y)

let rectangle_upper_segment (rect : rectangle) = 
  make_segment (make_point rect.lower_left.x rect.upper_right.y) rect.upper_right

let rectangle_left_segment (rect : rectangle) = 
  make_segment rect.lower_left (make_point rect.lower_left.x rect.upper_right.y)

let rectangle_right_segment (rect : rectangle) = 
  make_segment (make_point rect.upper_right.x rect.lower_left.y) rect.upper_right

let rectangle_perimeter (rect : rectangle) = 
  2. *. segment_length (rectangle_lower_segment rect) +. 
  2. *. segment_length (rectangle_left_segment rect)

let rectangle_area (rect : rectangle) = 
  segment_length (rectangle_lower_segment rect) *.
  segment_length (rectangle_left_segment rect)

let make_rectangle2 lx ly ux uy = 
  {lower_x = lx ; upper_x = ux ; lower_y = ly ; upper_y = uy}

let rectangle_lower_segment2 (rect : rectangle2) = 
  make_segment (make_point rect.lower_x rect.lower_y) (make_point rect.upper_x rect.lower_y)

let rectangle_upper_segment2 (rect : rectangle2) = 
  make_segment (make_point rect.lower_x rect.upper_y)  (make_point rect.upper_x rect.upper_y)

let rectangle_left_segment2 (rect : rectangle2) = 
  make_segment (make_point rect.lower_x rect.lower_y) (make_point rect.lower_x rect.upper_y)

let rectangle_right_segment2 (rect : rectangle2) = 
  make_segment (make_point rect.upper_x rect.lower_y) (make_point rect.upper_x rect.upper_y)

let rectangle_perimeter2 (rect : rectangle2) = 
  2. *. segment_length (rectangle_lower_segment2 rect) +. 
  2. *. segment_length (rectangle_left_segment2 rect)

let rectangle_area2 (rect : rectangle2) = 
  segment_length (rectangle_lower_segment2 rect) *.
  segment_length (rectangle_left_segment2 rect)

type ('a, 'b) pair = Pair of 'a * 'b
let first (Pair (x, _)) = x
let second (Pair (_, y)) = y

let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x _ -> x)

let second z = z (fun _ y -> y)

(*
Evaluate first (make pair x y)
  evaluate make pair x y -> fun m -> m x y
Evaluate first -> (fun x _ -> x)
Substitute fun m -> m x y into fun x _ -> x
Evaluate (fun x _ -> x) x y
x -> x
return x

Evaluate second (make_pair 1 2)
  evaluate make_pair 1 2
    evaluate 1 -> 1
    evaluate 2 -> 2
    evaluate make_pair -> fun m -> m x y
      substitute 1 for x and 2 for y
      evaluate m -> [primitive function m]
      apply m to 1, 2 -> 1 2

evaluate second 1 2
  evaluate 1 -> 1
  evaluate 2 -> 2
  evaluate second -> (fun _ y -> y) y
  substitute 1 for x and 2 for y
        apply fun _ y -> y to 1, 2 -> 2
        return 2
*)

(*A.4*)
let pow a b = 
  let rec pow_iter acc b = 
    match b with 
    | 0 -> acc
    | _ -> pow_iter (a * acc) (b - 1) in
  pow_iter 1 b

let rec int_log a b = 
    if b mod a <> 0
      then 0
  else 1 + int_log a (b / a)
    
let make_pairi a b = 
  pow 2 a * pow 3 b

let firsti pair =
int_log 2 pair

let secondi pair =
  int_log 3 pair

(*A.5*)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev = function
  | [] -> invalid_arg "Cannot represent negative numbers in Unary numbers"
  | _ :: t -> t

let rec integer_to_unary a = 
  if a = 0 
    then zero
else succ (integer_to_unary (a - 1))

let rec unary_to_integer u_int = 
  if is_zero u_int 
    then 0
else 1 + unary_to_integer (prev u_int)

let rec unary_add a b = 
  if is_zero b
    then a
else 
  unary_add (succ a) (prev b)

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' u = 
  match u with
    | Zero -> invalid_arg "Cannot represent negative numbers in Unary numbers"
    | Succ u' -> u' 

(*
Don't have to change for integer_to_unary because this function convert integer to unary
regardless of the underlying structure of unary  
*)
let rec integer_to_unary' a = 
  if a = 0 
    then zero'
  else succ' (integer_to_unary' (a - 1))

(*
Don't have to change for unary_to_integer because this function convert unary to integer
regardless of the underlying structure of unary  
*)
let rec unary_to_integer' u_int = 
  if is_zero' u_int 
    then 0
  else 1 + unary_to_integer' (prev' u_int)

(*
Don't have to change besides the name changes
*)
let rec unary_add' a b = 
  if is_zero' b
      then a
  else 
    unary_add' (succ' a) (prev' b)

(*A.6*)
(* zerof = "functional zero"; we call it this so as not to be confused with
   zero or zero' previously defined. *)

let zerof = fun s -> fun z -> z
   (* or equivalently: let zerof = fun s z -> z *)
   (* or equivalently: let zerof s z = z *)
 
let add1 n = fun s -> fun z -> s (n s z)
   (* or equivalently: let add1 n = fun s z -> s (n s z) *)
   (* or equivalently: let add1 n s z = s (n s z) *)

let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)

let church_to_integer a = a (fun x -> x + 1) 0

(*A.7*)
(*
We know that the type of church_to_integer is
val church_to_integer : ((int -> int) -> int -> 'c) -> 'c  
We can desugar this to 
((int -> int) -> (int -> 'c)) -> 'c  
(int -> (int -> 'c)) -> 'c  
where 'c can be any type

Now consider the type of zerof
val zerof : 'a -> 'b -> 'b
Desugar this to
'a -> ('b -> 'b)
where 'a and 'b can be any type

When zerof is inputted as argument, 'a and 'b is forced to become int, so
church_to_integer becomes (int -> (int -> int)) -> int 

Now consider the type of one
val one : ('a -> 'b) -> 'a -> 'b
We can rewrite this as 
('a -> 'b) -> 'a -> ('b -> 'b)
where 'a and 'b can be any type

When one is inputted as argument, 'a and 'b is forced to become int, so
church_to_integer becomes (int -> (int -> int)) -> int. 
*)

(*Part B*)

(*B1*)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch (Mobile (left, _)) = left
let right_branch (Mobile (_, right)) = right

let branch_length = function 
  | Weight (length, _) -> length
  | Structure (length, _) -> length

let branch_structure = function 
  | Weight (_, weight) -> `Weight weight
  | Structure (_, sub_structure) -> `Structure sub_structure

let rec branch_weight1 = function
  | Weight (_, weight) -> weight
  | Structure (_, sub_structure) -> total_weight1 sub_structure

and total_weight1 mobile_branch = 
  branch_weight1 (left_branch mobile_branch) + branch_weight1 (right_branch mobile_branch)

let rec branch_weight2 branch = 
  match branch_structure branch with 
    | `Weight weight -> weight
    | `Structure sub_structure -> total_weight2 sub_structure

  and total_weight2 mobile_branch = 
    branch_weight2 (left_branch mobile_branch) + branch_weight2 (right_branch mobile_branch)

  let rec is_balanced mobile_branch = 
    let left = left_branch mobile_branch 
    and right = right_branch mobile_branch in
    match (branch_structure left), (branch_structure right) with
    | `Weight _, `Weight _ ->  
      (branch_weight2 (left) * branch_length (left)) = (branch_weight2 (right) * branch_length (right))
    | `Weight _, `Structure sub_structure -> 
      (branch_weight2 (left) * branch_length (left)) = (branch_weight2 (right) * branch_length (right))
      && is_balanced sub_structure
    | `Structure sub_structure, `Weight _ -> 
      (branch_weight2 (left) * branch_length (left)) = (branch_weight2 (right) * branch_length (right))
      && is_balanced sub_structure
      | `Structure sub_structure1, `Structure sub_structure2 ->
        (branch_weight2 (left) * branch_length (left)) = (branch_weight2 (right) * branch_length (right))
      && is_balanced sub_structure1 && is_balanced sub_structure2

type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = {left = l ; right = r}
let make_weight' l w = Branch' (l, Weight' w) 
let make_structure' l m = Branch' (l, Structure' m)

let left_branch' mobile_branch = mobile_branch.left
let right_branch' mobile_branch = mobile_branch.right

let branch_length' (Branch' (l, _)) = l

let branch_structure' = function
  | Branch' (_, Weight' w) -> `Weight w
  | Branch' (_, Structure' m) -> `Structure m

let rec branch_weight' mobile_branch =
  match branch_structure' mobile_branch with
    | `Weight weight -> weight
    | `Structure sub_structure -> total_weight' sub_structure
  and total_weight' mobile_branch =
      branch_weight' (left_branch' mobile_branch) + branch_weight' (right_branch' mobile_branch)

  let rec is_balanced' mobile_branch = 
    let left = left_branch' mobile_branch 
    and right = right_branch' mobile_branch in
    match (branch_structure' left), (branch_structure' right) with
    | `Weight _, `Weight _ ->  
      (branch_weight' (left) * branch_length' (left)) = (branch_weight' (right) * branch_length' (right))
    | `Weight _, `Structure sub_structure -> 
      (branch_weight' (left) * branch_length' (left)) = (branch_weight' (right) * branch_length' (right))
      && is_balanced' sub_structure
    | `Structure sub_structure, `Weight _ -> 
      (branch_weight' (left) * branch_length' (left)) = (branch_weight' (right) * branch_length' (right))
      && is_balanced' sub_structure
      | `Structure sub_structure1, `Structure sub_structure2 ->
        (branch_weight' (left) * branch_length' (left)) = (branch_weight' (right) * branch_length' (right))
      && is_balanced' sub_structure1 && is_balanced' sub_structure2

(*B2*)
type tree = Tree of elem list
  and elem =
    | Num of int
    | Sub of tree

let rec square_tree (Tree lst) =
    let square_num = function
      | Num n -> Num (n * n)
      | Sub s -> Sub (square_tree s)
in
    let rec square_lst = function
    | [] -> []
    | h :: t -> square_num h :: square_lst t
in
Tree (square_lst lst)

let rec square_tree' (Tree lst) = 
  let square_num' = function
      | Num n -> Num (n * n)
      | Sub s -> Sub (square_tree' s)
in 
  Tree (List.map square_num' lst)

let rec tree_map map (Tree lst) = 
  let map_num = function
      | Num n -> Num (map n)
      | Sub s -> Sub (tree_map map s)
in 
  Tree (List.map map_num lst)

(*B2*)
let square_tree'' tree = tree_map (fun n -> n * n) tree

(*Part C*)

(*C1*)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)


let rec simplify1 expr = 
  match expr with
  | Add (Int a, Int b) -> Int (a + b)
  | Add (expr', Int 0) -> simplify1 expr'
  | Add (Int 0, expr') -> simplify1 expr'
  | Add (expr1', expr2') -> Add (simplify1 expr1', simplify1 expr2')

  | Mul (Int a, Int b) -> Int (a * b)
  |(Mul (_, Int 0) |  Mul (Int 0, _)) -> Int 0
  | (Mul (expr', Int 1) |  Mul (Int 1, expr')) -> simplify1 expr'
  | Mul (expr1', expr2') -> Mul (simplify1 expr1', simplify1 expr2')

  | Pow (_, 0) -> Int 1
  | Pow (Int 1, _) -> Int 1
  | Pow (Int base, exp) -> Int (int_of_float((float_of_int base) ** (float_of_int exp)))
  | Pow (expr', 1) -> simplify1 expr'
  | Pow (expr', n) -> Pow (simplify1 expr', n)

  | Int _ -> expr
  | Var _ -> expr

let rec simplify expr =
    let e = simplify1 expr in
      if expr = e
        then expr
        else simplify e

(*C2*)
let rec deriv var expr = 
  match expr with
  | Int _ -> Int 0
  | Var var' -> 
    if var' = var 
      then Int 1
  else Int 0
  | Add (expr1', expr2') -> Add (deriv var expr1', deriv var expr2')
  | Mul (expr1', expr2') -> Add (Mul (deriv var expr1', expr2'), Mul(expr1', deriv var expr2'))
  | Pow (expr', n) -> Mul(Mul (Int n, Pow (expr', (n - 1))), deriv var expr')

let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d