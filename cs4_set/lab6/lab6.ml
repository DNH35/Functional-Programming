(*Part A*)
(*A.1*)
(*
 FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *]

  FUNCTION 0 (factorial function)
    current env: FRAME 0
    param: n 
    body: 
      let rec iter m r =
        if m = 0
          then r
          else iter (m - 1) (r * m)
      in iter n 1

  FUNCTION 1 (iter function)
    current env: FRAME 0
    params: m, r
    body: 
      if m = 0
        then r
        else iter (m - 1) (r * m)
  
  FRAME 1 (factorial env)
    parent: FRAME 0 
    bindings:
        factorial: FUNCTION 0
  
  FRAME 2 (iter env)
    parent: FRAME 0
    bindings:
        iter : FUNCTION 1

  FRAME 3 (factorial call env)
    parent: None
    bindings:
        n : 3
  
  FRAME 4 (iter call 1 env)
    parent: FRAME 2
    bindings:
        m : 3
        r : 1
  
  FRAME 5 (iter call 2 env)
    parent: FRAME 2
    bindings:
        m : 2
        r : 3

  FRAME 6 (iter call 3 env)
    parent: FRAME 2
      bindings: 
        m : 1
        r : 6
  
  FRAME 7 (iter call 4 env)
    parent: FRAME 2
      bindings: 
        m : 0
        r : 6
*)

(*A.2*)
let factorial =
  let f = ref (fun _ -> 0) in
      (* to be completed *)
      f := (fun n -> if n = 0 then 1 else n * !f (n - 1));
      !f

(*Part B*)

(*B.1*)
exception Stat_error of string

let make_stat_1 () = 
  let sum = ref 0. in 
  let sumsq = ref 0. in
  let n = ref 0 in 
  object 
    method append ele = 
      sum := !sum +. ele;
      sumsq := !sumsq +. (ele *. ele);
      n := !n + 1;

    method mean = 
      if !n = 0
        then raise (Stat_error("need at least one value for mean"))
    else !sum /. float_of_int(!n)

    method variance = 
      if !n = 0
        then raise (Stat_error("need at least one value for variance"))
    else (!sumsq -. (!sum ** 2. /. float_of_int(!n))) /. float_of_int(!n)

    method stdev = 
      if !n = 0
        then raise (Stat_error("need at least one value for stdev"))
    else sqrt((!sumsq -. (!sum ** 2. /. float_of_int(!n))) /. float_of_int(!n))

    method clear = 
      sum := 0.;
      sumsq := 0.;
      n := 0;
    end 

let make_stat_2 () = 
  let sum = ref 0. in 
  let sumsq = ref 0. in
  let n = ref 0 in 
  object(self) 
    method append ele = 
      sum := !sum +. ele;
      sumsq := !sumsq +. (ele *. ele);
      n := !n + 1;

    method mean = 
      if !n = 0
        then raise (Stat_error("need at least one value for mean"))
    else !sum /. float_of_int(!n)

    method private _variance = 
      (!sumsq -. (!sum ** 2. /. float_of_int(!n))) /. float_of_int(!n)
    method variance = 
      if !n = 0
        then raise (Stat_error("need at least one value for variance"))
    else self#_variance

    method stdev = 
      if !n = 0
        then raise (Stat_error("need at least one value for stdev"))
    else sqrt(self#variance)

    method clear = 
      sum := 0.;
      sumsq := 0.;
      n := 0;
    end 

let make_stat_2 () = 
  let sum = ref 0. in 
  let sumsq = ref 0. in
  let n = ref 0 in 
  object(self) 
    method append ele = 
      sum := !sum +. ele;
      sumsq := !sumsq +. (ele *. ele);
      n := !n + 1;

    method mean = 
      if !n = 0
        then raise (Stat_error("need at least one value for mean"))
    else !sum /. float_of_int(!n)

    method private _variance = 
      (!sumsq -. (!sum ** 2. /. float_of_int(!n))) /. float_of_int(!n)
    method variance = 
      if !n = 0
        then raise (Stat_error("need at least one value for variance"))
    else self#_variance

    method stdev = 
      if !n = 0
        then raise (Stat_error("need at least one value for stdev"))
    else sqrt(self#variance)

    method clear = 
      sum := 0.;
      sumsq := 0.;
      n := 0;
    end 

(*B.2*)
class stat = 
  let sum = ref 0. in 
  let sumsq = ref 0. in
  let n = ref 0 in 
  object(self) 
    method append ele = 
      sum := !sum +. ele;
      sumsq := !sumsq +. (ele *. ele);
      n := !n + 1;

    method mean = 
      if !n = 0
        then raise (Stat_error("need at least one value for mean"))
    else !sum /. float_of_int(!n)

    method private _variance = 
      (!sumsq -. (!sum ** 2. /. float_of_int(!n))) /. float_of_int(!n)
    method variance = 
      if !n = 0
        then raise (Stat_error("need at least one value for variance"))
    else self#_variance

    method stdev = 
      if !n = 0
        then raise (Stat_error("need at least one value for stdev"))
    else sqrt(self#variance)

    method clear = 
      sum := 0.;
      sumsq := 0.;
      n := 0;
    end 

(*Part C*)

(*C.1*)
module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end


module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t

    (* Your code goes here. *)
    let empty = Leaf
    let is_empty h = 
      match h with 
      | Leaf -> true
      | Node _ -> false 

    let find_min h = 
      match h with 
      | Leaf -> raise Empty
      | Node (_, x, _, _) -> x
    
      let rec merge h1 h2 =
        match (h1, h2) with
        | h, Leaf | Leaf, h -> h
        | Node (r1, x1, lh1, rh1), Node (r2, x2, _, _) ->
          if x1 <= x2 
            then 
              let merged = merge rh1 h2 in

              let left = 
                if r1 >= r2 
                  then lh1
              else merged in

              let right = 
                if r1 < r2 
                  then rh1
              else merged in 

              let min_rank = min r1 r2 in 
              Node (min_rank + 1, x1, left, right)

        else merge h2 h1

    let insert h ele = merge (Node (1, ele, Leaf, Leaf)) h

    let delete_min = function
      | Leaf -> raise Empty
      | Node(_, _, lh, rh) -> merge lh rh

    let from_list lst = 
      let rec from_list_helper pq = function 
        | [] -> pq
        | h :: t -> from_list_helper (insert pq h) t
    in from_list_helper empty lst
  end

let heap_sort lst = 
  let module PQ = PriorityQueue in 
  let rec heap_sort_helper acc pq = 
    if PQ.is_empty pq
      then List.rev acc 
  else 
    let min_ele = PQ.find_min pq in
    let delete_min_pq = PQ.delete_min pq in
    heap_sort_helper (min_ele :: acc) delete_min_pq in 
  heap_sort_helper [] (PQ.from_list lst)

(*Part C.2*)
module MakePriorityQueue (Elt : ORDERED_TYPE)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty

    type elem = Elt.t

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t

    (* Your code goes here. *)
    let empty = Leaf
    let is_empty h = 
      match h with 
      | Leaf -> true
      | Node _ -> false 

    let find_min h = 
      match h with 
      | Leaf -> raise Empty
      | Node (_, x, _, _) -> x
    
      let rec merge h1 h2 =
        match (h1, h2) with
        | h, Leaf | Leaf, h -> h
        | Node (r1, x1, lh1, rh1), Node (r2, x2, _, _) ->
          if x1 <= x2 
            then 
              let merged = merge rh1 h2 in

              let left = 
                if r1 >= r2 
                  then lh1
              else merged in

              let right = 
                if r1 < r2 
                  then rh1
              else merged in 

              let min_rank = min r1 r2 in 
              Node (min_rank + 1, x1, left, right)

        else merge h2 h1

    let insert h ele = merge (Node (1, ele, Leaf, Leaf)) h

    let delete_min = function
      | Leaf -> raise Empty
      | Node(_, _, lh, rh) -> merge lh rh

    let from_list lst = 
      let rec from_list_helper pq = function 
        | [] -> pq
        | h :: t -> from_list_helper (insert pq h) t
    in from_list_helper empty lst
  end

module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then 0 else if x < y then -1 else 1
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst =
  let module PQ = StringPQ in
  let rec heap_sort_helper acc pq =
    if PQ.is_empty pq then
      List.rev acc
    else
      let min_ele = PQ.find_min pq in
      let delete_min_pq = PQ.delete_min pq in
      heap_sort_helper (min_ele :: acc) delete_min_pq
  in
  heap_sort_helper [] (PQ.from_list lst)


(*Part D*)

(*D.1*)
type 'a contents = 
    | Delayed of (unit -> 'a) (*Delayed computation*)
    | Value of 'a (*value when forced call*)

type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Delayed e)
let force lz = 
  match !lz with 
  | Value v -> v
  | Delayed f -> 
    let x = f () in 
    lz := Value x;
    x

(*D.2*)
let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

let almost_sum = 
  fun f -> 
    fun lst -> 
      match lst with 
      | [] -> 0
      | h :: t -> h + f t

let sum lst = y almost_sum lst

let factorial =
  fun iter ->
    fun (n, r) ->
    if n = 0
      then r
      else iter(n - 1, n * r)

let factorial2 n = y factorial (n, 1)