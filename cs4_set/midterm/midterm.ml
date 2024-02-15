(*Part A*)

(*A.1*)
(*
Time Complexity: O(n)
The function f calls aux which is a tail recursive function. Notice that
the number of times aux calls itself is linear with respect to n because it decrements n
whenever it makes a recursive call. Thus, the time complexity is O(n).
*)

(*A.2*)

(*
Time Complexity: O(|m - n|)
The function terminates when n = m and the function calls itself to increment or decrement 
m until n = m. The worst case scenerio is when m and n are further apart, which makes the function 
time complexity to be the absolute of the difference of m and n.
*)

(*A.3*)

(*
Time Complexity: O(n)
The function trib calls its helper method iter which is a tail recursive function. Notice that
whenever iter computes the next number in the sequence, iter calls itself, storing the new element
in one of the parameter and decrement n. Thus, the number of times iter calls itself is linear with 
respect to n. 
*)

(*A.4*)

(*
Time Complexity: O(log(n))
Consider all the cases:
When n <= 1, the function simply terminates and return 1
When n is divisible by 3, the function will call itself and divides n by 3, so in the case when n
continuously divided by 3 until it reaches to 1, the time complexity will be O(log(n))
When n has a remainder of 1 when divided by 3 or otherwise, the function will make a recursive call
and decrement n by 1 or 2, respectively. So in the case when n is not divisible by 3, the time complexity is 
O(n).
Notice that even in the worst case which is when we have to decrement n by 1 or 2 every time for n to be divisible by
3, we would have a time complexity of O(log(n) - a) when a is a constant representing the total number of time we have
decrement n to be divisible by 3, we would still have a time complexity of O(log(n)).
*)

(*A.5*)

(*
We cannot analyze its time complexity because for n > 1, if n is even, f will make a recursive call
and halve n, otherwise make a recursive call on 3 * n + 1. Given that the function terminates at 1, we are
unable to guaranteer convergence as the we do not know the behavior of this sequence since we're halving it 
when it's even and increasing it by a factor and a constant when it's odd. 
*)
(*Part B*)

(*B.1*)

let rec is_prefix lst1 lst2 = 
  match (lst1, lst2) with
    | ([], _) -> true
    | (_, []) -> false
    | ((h1 :: t1), (h2 :: t2)) -> 
      if h1 = h2 
        then is_prefix t1 t2
    else false

let rec is_subsequence lst1 lst2 =
    if lst1 = []
      then true
  else
    match lst2 with
    | ([]) -> false
    | (_) when is_prefix lst1 lst2 -> true
    | _ :: t2 -> is_subsequence lst1 t2

(*B.2*)
let rec is_embedded lst1 lst2 = 
  match (lst1, lst2) with
    | ([], _) -> true
    | (_, []) -> false
    | ((h1 :: t1), (h2 :: t2)) when h1 = h2 -> is_embedded t1 t2
    | (_, _ :: t2) -> is_embedded lst1 t2

let remove_first ele lst = 
  let rec remove_first_helper ele acc = function
  | [] -> None
  | h :: t -> 
    if ele = h 
      then Some (acc @ t)
  else remove_first_helper ele (acc @ [h]) t
in remove_first_helper ele [] lst

(*B.3*)
let rec same_elements lst1 lst2 = 
  match lst1 with
    | [] -> lst2 = []
    | (h :: t) -> 
      match remove_first h lst2 with
        | Some t' -> same_elements t t'
        | None -> false

(*B.4*)
let repeated ele lst = 
  match remove_first ele lst with
    | Some t' -> 
      if remove_first ele t' = None
        then false
    else true
    | None -> false

let rec any f lst = 
  match lst with
    | [] -> false
    | h :: t -> 
      if f h 
        then true
    else any f t


let rec all f lst = 
  match lst with
    | [] -> true
    | h :: t -> 
      if f h 
        then all f t
    else false

let any_repeated lst = 
  any (fun x -> repeated x lst) lst

let all_repeated lst = 
    all (fun x -> repeated x lst) lst

let id x = x

(*B.5*)
let compose f g x = f (g x)

let rec compose_all = function 
  | [] -> id 
  | [f] -> f
  | h :: t -> compose h (compose_all t)

let compose_all2 lst = List.fold_right compose lst id

(*
compose_all function has type signature of ('a -> 'a) list -> ('a -> 'a) because
it takes in a list of functions that perform the mapping ('a -> 'a), and the resulting
output has the type of 'a -> 'a because each function in the list takes in input and provides
output of the same type 'a. 

compose_all2 function has type signature of ('a -> 'a) list -> ('a -> 'a) because
it takes in a list of functions that perform the mapping ('a -> 'a). Notice that
List.fold_right has the type signure of ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc.
Notice that the function compose and id has a type signature 'a -> a', so it Ocaml interpreter
will force 'acc to be 'a, resulting the type signature of compose_all2 to be ('a -> 'a) list -> ('a -> 'a)

Now, we cannot use it on functions with more general types i.e. 'a -> 'b where 'a and 'b are different. Consider
two functions of signatures 'a -> 'b and 'c -> 'd. Performing composing operator on these two functions 
would be invalid. 
   
*)

(*Part C*)

(* Implementation of weight-balanced trees in OCaml. *)

let tree_debug = ref true

(* Parameters.
 * - delta is used to decide if any rotation needs to be made at all
 * - gamma is used to decide between a single and a double rotation
 *)
let delta = 3
let gamma = 2

type tree =
  | Leaf
  | Node of int * int * tree * tree  (* size, value, left/right subtrees *)

let get_size = function
  | Leaf -> 0
  | Node (s, _, _, _) -> s

let empty = Leaf

let singleton v = Node (1, v, Leaf, Leaf)

(* Build a node with the correct weight
   from a value `v`, a left subtree `l`,
   and a right subtree `r`. *)
let node v l r =
  let w = get_size l + get_size r + 1 in
    Node (w, v, l, r)

(* Are two subtrees balanced with respect to each other
 * i.e. "relatively balanced"?
 * Criterion:
 *   The "weight" of subtree b is <=
 *   delta times the "weight" of subtree a.
 *   "weight" is just the size + 1.
 *)
let rel_balanced a b =
  let wa = get_size a + 1 in
  let wb = get_size b + 1 in
    wb <= delta * wa

(* Is a single rotation indicated?
 * Criterion:
 *   The weight of subtree a is less than gamma times
 *   the weight of subtree b.
 *)
let need_single_rot a b =
  let wa = get_size a + 1 in
  let wb = get_size b + 1 in
    wa < gamma * wb

(* Find the minimum element in a tree.
 * This assumes that the tree is ordered. *)
let rec min_tree = function
  | Leaf -> None
  | Node (_, i, l, _) ->
    begin
      match min_tree l with
        | None -> Some i
        | Some l' -> Some l'
    end

(* Find the maximum element in a tree.
 * This assumes that the tree is ordered. *)
let rec max_tree = function
  | Leaf -> None
  | Node (_, i, _, r) ->
    begin
      match max_tree r with
        | None -> Some i
        | Some r' -> Some r'
    end

(* Is a tree ordered? *)
let rec ordered = function
  | Leaf -> true
  | Node (_, i, l, r) ->
    begin
      ordered l &&
      ordered r &&
      match (max_tree l, min_tree r) with
        | (None, None) -> true
        | (Some l', None) -> l' < i
        | (None, Some r') -> i < r'
        | (Some l', Some r') -> l' < i && i < r'
    end

(* Is a tree balanced with respect to the weight criteria? *)
let rec balanced = function
  | Leaf -> true
  | Node (_, _, l, r) ->
      rel_balanced l r &&
      rel_balanced r l &&
      balanced l &&
      balanced r

(* ----------------------------------------------------------------------
 * Tree rotations and rebalancing.
 * ---------------------------------------------------------------------- *)

(*C.2*)
let single_l v l r =
  if !tree_debug then
    Printf.printf "- single_l at tree rooted at %d\n" v;
  match r with
    | Leaf -> invalid_arg "single_l"
    | Node (_, rv, rl, rr) ->
      if get_size rl + 1 > delta * (get_size rr + 1) then
        match rl with
        | Leaf -> invalid_arg "single_l"
        | Node (_, rlv, rll, rlr) ->
          node rlv (node rv l rll) (node v rlr rr)
      else
        node rv (node v l rl) rr

(*C.4*)
let double_l v l r =
  if !tree_debug then
    Printf.printf "- double_l at tree rooted at %d\n" v;
  match r with
    | Leaf
    | Node (_, _, Leaf, _) -> invalid_arg "double_l"
    | Node (_, rv, rl, rr) ->
      match rl with
      | Leaf -> invalid_arg "double_l"
      | Node (_, rlv, rll, rlr) ->
        let new_l = node v l rll in
        let new_r = node rv rlr rr in
        node rlv new_l new_r

let rotate_l v l r =
  if !tree_debug then
    Printf.printf "rotate_l at tree rooted at %d\n" v;
  match r with
    | Leaf -> invalid_arg "rotate_l"
    | Node (_, _, rl, rr) ->
      if need_single_rot rl rr then
        single_l v l r
      else
        double_l v l r

(*C.3*)
let single_r v l r =
  if !tree_debug then
    Printf.printf "- single_r at tree rooted at %d\n" v;
  match l with
    | Leaf -> invalid_arg "single_r"
    | Node (_, lv, ll, lr) ->
      if get_size lr + 1 > delta * (get_size ll + 1) then
        match lr with
        | Leaf -> invalid_arg "single_r"
        | Node (_, lrv, lrl, lrr) ->
          node lrv (node v ll lrl) (node lv lrr r)
      else
        node lv ll (node v lr r)

(*C.5*)
let double_r v l r =
  if !tree_debug then
    Printf.printf "- double_r at tree rooted at %d\n" v;
  match l with
    | Leaf
    | Node (_, _, _, Leaf) -> invalid_arg "double_r"
    | Node (_, lv, ll, lr) ->
      match lr with
        | Leaf -> invalid_arg "double_r"
        | Node (_, lrv, lrl, lrr) ->
          let new_l = node lv ll lrl in
          let new_r = node v lrr r in
          node lrv new_l new_r

let rotate_r v l r =
  if !tree_debug then
    Printf.printf "rotate_r at tree rooted at %d\n" v;
  match l with
    | Leaf -> invalid_arg "rotate_r"
    | Node (_, _, ll, lr) ->
      if need_single_rot lr ll then
        single_r v l r
      else
        double_r v l r

(* ----------------------------------------------------------------------
 * Membership testing.
 * ---------------------------------------------------------------------- *)

(* Is a value a member of a tree? *)
(*C.1*)
let rec tree_member v t = 
  match t with 
    | Leaf -> false
    | Node (_, v', l, r) ->
      if v = v'
        then true
      else 
        if v < v'
          then tree_member v l
        else 
          tree_member v r


(* ----------------------------------------------------------------------
 * Inserting values into trees.
 * ---------------------------------------------------------------------- *)

let balance_l v l r =
  if rel_balanced l r then
    node v l r
  else
    rotate_l v l r

let balance_r v l r =
  if rel_balanced r l then
    node v l r
  else
    rotate_r v l r

(* Insert a value into a tree with rebalancing. *)
let rec insert v t =
  match t with
    | Leaf -> singleton v
    | Node (_, v', _, _) when v = v' -> t
    | Node (_, v', l, r) ->
        if v < v' then
          (* Adding to the left means you may have to rotate to the right. *)
          balance_r v' (insert v l) r
        else
          (* Adding to the right means you may have to rotate to the left. *)
          balance_l v' l (insert v r)

(* Convert a list into a tree *)
let tree_of_list lst =
  List.fold_left (fun t x -> insert x t) empty lst

(* Seed the random number generator. *)
let _ = Random.self_init ()

(* Generate a list of random integers.
 * Pick `n` random ints between `0` and `imax - 1`. *)
let random_list n imax =
  let rec iter n lst =
    if n = 0 then
      lst
    else
      iter (n - 1) (Random.int imax :: lst)
  in
    iter n []

(* Generate a tree of random integers.
 * Pick `n` random ints between `0` and `imax - 1`
 * and add them to the tree one after another. *)
let random_tree n imax = tree_of_list (random_list n imax)

(* Pretty-print a tree. *)
let print_tree tree =
  let blanks n = String.make n ' ' in
  let rec aux tree indent =
    let ind = blanks indent in
      match tree with
        | Leaf -> Printf.printf "%sLeaf\n" ind
        | Node (d, v, l, r) ->
          begin
            Printf.printf "%sNode[(%d) [size %d]\n" ind v d;
            aux l (indent + 2);
            Printf.printf "%s  ----\n" ind;
            aux r (indent + 2);
            Printf.printf "%s]\n" ind;
          end
  in
    aux tree 0