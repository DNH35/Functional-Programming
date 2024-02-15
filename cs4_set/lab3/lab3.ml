(* 
Part A   
*)

let rec range m n = 
  if m > n 
    then []
else m :: range (m + 1) n

let irange m n = 
  let rec irange_helper lst m n = 
    if n < m
      then lst
  else irange_helper (n :: lst) m (n - 1)
in
irange_helper [] m n

let rec last_sublist = function
    | [] -> invalid_arg("last_sublist: empty list")
    | [h] ->  [h]
    | _ :: t -> last_sublist t

let reverse lst = 
  let rec reverse_helper rev_lst lst =
    match lst with 
    | [] -> rev_lst
    | h :: t -> reverse_helper (h :: rev_lst) t 
  in 
  reverse_helper [] lst

let rec square_list = function
  | [] -> []
  | h :: t -> h * h :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(*
Because the :: operator appends an element to the front and answer starts out
empty so h * h will be accumulated in reverse order. 

Switching the order would not work because :: takes in an element as its first argument
and a list as its second argument and append the element in the front. 

The new change would not be time efficient because @ operation is O(n) while :: is O(1)
*)

let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [(h * h)])
  in iter items []

let count_negative_numbers lst = 
  let rec count_negative_numbers_helper count = function
    | [] -> count
    | h :: t -> 
      if h < 0 
        then count_negative_numbers_helper (count + 1) t
    else count_negative_numbers_helper (count) t
  in count_negative_numbers_helper 0 lst

let rec power_of_two_list n = 
  let rec pow_2 n =
    if n = 0 
      then 1
  else 2 * pow_2 (n - 1)
in 
match n with
| 0 -> []
| _ -> power_of_two_list (n - 1) @ [pow_2 (n - 1)] 

let prefix_sum lst = 
  let rec prefix_sum_helper acc n = 
    function
    | [] -> acc
    | h :: t -> prefix_sum_helper (acc @ [(n + h)]) (n + h) t
  in
  prefix_sum_helper [] 0 lst

let deep_reverse lst = 
  let rec deep_reverse_helper rev_lst = function
    | [] -> rev_lst
    | h :: t -> deep_reverse_helper (reverse(h) :: rev_lst) t
  in deep_reverse_helper [] lst

type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

let rec deep_reverse_nested lst = 
  match lst with
  | Value _ -> lst
  | List l -> 
    let rec deep_reverse_nested_helper rev_lst = function
    | [] -> rev_lst
    | h :: t -> deep_reverse_nested_helper (deep_reverse_nested h :: rev_lst) t
  in
   List (deep_reverse_nested_helper [] l)

  (*Part B*)
let rec quicksort comp lst = 
  match lst with 
    | [] -> []
    | h :: t -> 
      quicksort comp (List.filter (fun x -> comp x  h) t) @ (h :: quicksort comp (List.filter (fun x -> not (comp x h)) t))

(*
Quicksort is generative recursion because it recursively sort on the list with elements 
that are greater than the pivot and the list with elements that are less than the pivots. Notice
that these lists aren't part of the original list but the lists that we generated. 
*)

(* let rec odd_half a_list =
  match a_list with
    | [] -> []
    | [x] -> [x]  (* copy 1-element list *)
    | h :: _ :: t -> h :: odd_half t (* skip second element in list *)

let even_half a_list =
  match a_list with
    | [] -> []
    | _ :: t -> odd_half t

let rec merge_in_order list1 list2 cmp =
  match (list1, list2) with
   | ([], _) -> list2
   | (_, []) -> list1
   | (h1 :: t1, h2 :: _) when cmp h1 h2 ->
       h1 :: merge_in_order t1 list2 cmp
   | (_, h2 :: t2) ->
       h2 :: merge_in_order list1 t2 cmp

let rec merge_sort a_list cmp =
  match a_list with
    | []
    | _ ->
      let eh = even_half a_list in
      let oh = odd_half a_list in
        merge_in_order
          (merge_sort eh cmp)
          (merge_sort oh cmp) cmp *)
(*
This won't work because for merge sort will break the list down until there's separate
lists with each one has single node before recursively sort and merge the lists. Thus, there
should be a base case to check if there's one element in the list. 
*)

let rec insert_in_order cmp new_result a_list =
  match a_list with
    | [] -> [new_result]
    | h :: _ when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order cmp new_result t

let rec insertion_sort cmp a_list =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order cmp h (insertion_sort cmp t)

(*
Insertion sort is structural recursion because the recursive process sort the list
simply by transversing the given data   
*)

let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun x -> h :: x) rest)

(*The algorithm works by appending h to each subset of t and concatenate subsets of t
with the newly created subsets of t with h appended to it*)

let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> p x :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun _ r -> r + 1) 0 sequence

let rec accumulate_n op init seqs =
    match seqs with
      | [] -> failwith "empty list"
      | [] :: _ -> []   (* assume all sublists are empty *)
      | _ ->  (* non-empty list containing non-empty sublists *)
          accumulate op init (List.map List.hd seqs)::(accumulate_n op init (List.map List.tl seqs))

let rec map2 f x y =
    match (x, y) with
      | ([], []) -> []
      | ([], _) -> failwith "unequal lists"
      | (_, []) -> failwith "unequal lists"
      | (h1 :: t1, h2 :: t2) -> f h1 h2 :: map2 f t1 t2

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)
          
let matrix_times_vector m v = map (fun x -> dot_product x v) m
          
let transpose mat = accumulate_n (fun x r -> x :: r) [] mat
          
let matrix_times_matrix m n =
    let cols = transpose n in
               map (fun r -> matrix_times_vector cols r) m