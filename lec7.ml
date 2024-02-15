let make_result x y = (x, y)

let get_x (x, _) = x

let get_y (y, _) = y

let test_data = [make_result 1 1.9; make_result 2 4.3; make_result 3 6.0; make_result 4 7.8]

let rec remove_by_x_r (x : int) lst = 
  match lst with
  | [] -> []
  | h :: t when get_x h = x -> t (*remove_by_x_r x t*)
  | h :: t -> h :: remove_by_x_r x t

let rec fold_left f init lst = 
  match lst with
    | [] -> []
    | h :: t -> fold_left f (f init h) t

let remove_by_x lst x =
  (* List.filter (fun h -> get_x h <> x) lst *)
  (List.fold_left
    (fun r h -> if get_x h = x then r else h :: r)
    []
    lst)

let rec fold_left_or_quit f is_done g init lst = 
  match lst with
    | [] -> init
    | h :: t ->
      if is_done h then
        g init h t
      else fold_left_or_quit f is_done g (f init h) t

type result_t = (int * float)

let rec insert_in_order (item : result_t) (lst : result_t list) =
  match lst with
    | [] -> [item]
    | h :: t ->
        if get_x item < get_x h then
          item :: lst
        else 
          h :: insert_in_order item t

let rec insert_in_order_by cmp item lst =
  match lst with
    | [] -> [item]
    | h :: t ->
                  (* if get_x item < get_x h then *)
      if cmp item h < 0 then
          item :: lst
        else 
          h :: insert_in_order_by cmp item t

let insert_in_order2 item lst = 
  insert_in_order_by
    (fun a b -> compare (get_x a) (get_x b)) item lst

let insertion_sort a_list cmp = 
  let rec iter unsorted sorted = 
    match unsorted with
      | [] -> sorted
      | h :: t ->
        iter t (insert_in_order_by cmp h sorted)
      in iter a_list []
let rec merge cmp lst1 lst2 = 
  match (lst1, lst2) with
  | ([], []) -> []
  | ([] _) -> lst2
  | (_, []) -> lst1
  | (h1 :: t1, h2 :: t2) ->
    if cmp h1 h2 < 0 then
      h1 :: merge cmp t1 lst2
    else h2 :: merge cmplst1 t2
    
let rec merge_sort cmp lst1 lst2 = 
  match lst with 
  | [] -> []
  | [x] -> [x]
  | _ ->
    let (l1, l2) = split lst in
    let l1' = merge_sort cmp l1 in
    let l2' = merge_sort cmp l2 in
    merge cmp l1' l2' 