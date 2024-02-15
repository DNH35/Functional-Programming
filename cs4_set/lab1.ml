(*
Duy Nguyen
*)
(*Part A*)
let rec sum_squares_to n = 
  if n = 0 then 0 else (n * n) + sum_squares_to(n - 1);;

  (* 
  1. Expressions:
  
  1. int = 10
  2. float = 10.
  3. int = 12
  4. Error: This expression has type float but an expression was expected of type
         int
     Because (+) only takes in int types and produce int output, and OCaml is strictly typed, so we 
  5. Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `3.'?
    Because (+) only takes in float types and produce float output, and it reads
    in 3 first, so the hint only provides hint for 3
  6. Error: This expression has type float but an expression was expected of type
         int
     Both types for (+) has to be int because it is performing the mapping int->int->int
  7. Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `3.'?
    Both types for (+.) must be float
  8. 7.2
  9. 5
  10. 7
  11. val a : int = 3
  12. val b : int = 4
  13. bool = false
  14. bool = true
  15. No because == is used to compared identical while = is used to compare structurally equal
  16. (int * int * int) list = [(1, 2, 3)]
  17. (int * int * int) list = [(1, 2, 3)]
      This gives a list with one element that is a tuple of (1,2,3) because we're using , to separate
      the elements instead of ;
  18. int = 4
  19. Error: Syntax error 
      Because we have to use && to compare values since and is not proper syntax
  20. int = 6
  21. int = 4
      because + 2 now belongs to the part of the else statement instead of 20 which performs
      and addition operation based on the condition
  22. int = 6
  23. Error: This expression has type int but an expression was expected of type 
       unit because it is in the result of a conditional with no else branch
       Because we only can use if experssion without else only when the expression returns a unit,
        which is untrue in this case, so it is a type error
  *)

  let sum_of_squares_of_two_largest a b c = 
    if a >= b && a >= c then
      if b >= c then
          a*a + b*b
      else
          a*a + c*c
  else 
    if b >= a && b >= c then
      if a >= c then
          a*a + b*b
      else
          b*b + c*c
  else
      if a >= b then
          a*a + c*c
      else
          b*b + c*c
    ;;

  let a_plus_abs_b a b =
      (if b > 0 then (+) else (-)) a b

  (*
  The function evaluates the two input number and add a + b when b is positive and performs
  a - b when b is negative, which is the same as a + abs(b).     
  *)

(*Part B*)

(*
1.
  In Applicative Order evaluation, the argument 0 is first evaluated, resulting in 0.
  Then, the argument (p ()) is evaluated, which will lead to an infinite loop, so the interpreter will run into calling
  p () infintely  

    Normal order interpreter will evaluate 0 first, so it will go through the if statement and since x in this case
    is 0, so it will return 0 and terminate. 

2.
    The function will get stuck. Consider the substition model: when new if is evaluated, the compiler will try to
    evaluate all the given arguments so
      is_good_enough guess x is evaluated to abs_float (square guess -. x) < 0.00001
      guess is evaluated to guess
      However, notice that sqrt_iter (improve guess x) x is calling itself so it will be evaluating itself continously,
      resulting in an infinite loop. 

3. 
  1. 
  The function add_a is generative recursive because the inc operation is deferred and stays on the stack meanwhile the function
  add_b is an iterative process because it not only runs in linear in time but it also runs in constant stack space since
  there is no pending operation stays on the stack. 

let rec add_a a b =
  if a = 0
     then b
     else inc (add_a (dec a) b)

Desugar this to:

let rec add_a =
  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Evaluate (add_a 2 5)
  evaluate 2 to 2
  evaluate 5 to 5
  evaluate add_a -> fun ...
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else inc (add_a (dec a) b)

  evaluate (if 2 = 0 then 5 else inc (add_a (dec a) b)
    if will be evaluated first:
      evaluate (2 = 0)
        evaluate 2 to 2
        evaluate 0 to 0
        evaluate = to [primitive funciton =]
        apply = to 2, 0 -> false
    Evaluates to false, so evaluate the third operand:
      evaluate inc (add_a (dec a) b)

      evaluate add_a (dec 2) 5 
          evaluate (dec 2)
            evaluate 2 to 2
            evaluate dec to [primitive function dec]
            apply dec to 2 -> 1
          evaluate 5 to 5
        apply (fun a b -> if ...) to 1, 5
      
      Deferred inc() operation

        substitute 1 for a, 5 for b in (if ...)
          -> if 1 = 0 then 5 else inc((add_a (dec 1) 5))
        evaluate (if 1 = 0 then 5 else inc((add_a (dec 1) 5))
          if will be evaluated first:
            evaluate (1 = 0)
              evaluate 1 to 1
              evaluate 0 to 0
              evaluate = to [primitive function =]
              apply = to 1, 0 -> false

         Evaluates to false, so evaluate the third operand:
            evaluate inc((add_a (dec 1) 5))
              
              evaluate add_a (dec 1) 5
                evaluate (dec 1)
                  evaluate 1 to 1
                  evaluate dec to [primitive function dec]
                  apply dec to 1 -> 0
                evaluate 5
              apply (fun a b -> if ...) to 0, 5

              Deferred inc(inc()) operation

              substitute 0 for a, 5 for b in (if ...)
                -> if 0 = 0 then 5 else inc((add_a 0 5))
              evaluate (if 0 = 0 then 5 else inc(add_b (dec 0) 5))
                if will be evaluated first::
                  evaluate (0 = 0)
                    evaluate 0 to 0
                    evaluate 0 to 0
                    evaluate = to [primitive funciton =]
                    apply = to 0, 0 -> true
                Evaluates to true, so
                  evaluate 5 -> 5

                Return 5
                Evaluate inc -> [primitive function inc]
                apply inc to 5 -> 6
                Return 6
              Evaluate inc -> [primitive function inc] 
              apply inc(6) -> 7
              Result: 7


(*
let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
      >>> Evaluate 2 to 2
      >>> evaluate 0 to 0
      >>> evaluate = to [primitive funciton =]
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
        >>> evaluate 2 to 2
        >>> evaluate dec to [primitive function dec]
          apply dec to 2 -> 1
        evaluate (inc 5)
        >>> Evaluate 5 to 5
        >>> evaluate inc to [primitive function inc]
          apply inc to 5 -> 6
        >>> evaluate add_b -> (fun a b -> if ...)
        apply (fun a b -> if ...) to 1, 6
        substitute 1 for a, 6 for b in (if ...)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
          >>> Evaluate 1 to 1
          >>> evaluate 0 to 0
          >>> evaluate = to [primitive funciton =]
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
              >>> Evaluate 1 to 1
              >>> evaluate dec to [primitive function dec]
                apply dec to 1 -> 0
              evaluate (inc 6)
              >>> Evaluate 6 to 6
              >>> evaluate inc to [primitive function inc]
                apply inc to 6 -> 7
              >>> evaluate add_b -> (fun a b -> if ...)
              apply (fun a b -> if ...) to 0, 7
              substitute 0 for a, 7 for b in (if ...)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                >>> Evaluate 0 to 0
                >>> evaluate 0 to 0
                >>> evaluate = to [primitive funciton =]
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand:
                  >>>evaluate 7 -> 7
                  result: 7
*)
*)

(*Part C*)

(* This function computes the factorial of the input number,
   which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

(*a*)
let e_term n = 1. /. float_of_int(factorial n)

(*b*)
let rec e_approximation n =
  match n with
    | 0 -> 1.
    | _ -> e_term n +. e_approximation (n - 1)

(*
c.
e_approximation 20 = 2.71828182845904553   
exp 1.0 =            2.71828182845904509
*)

(*
d. As the term gets too high, it is harder for the compiler to compute the precision of such small numbers,
so the term will be evaluated to 0 and 1/0 goes to infininty.   
*)

(*2*)
let rec is_even n =
  match n with
    | 0 -> true
    | _ -> is_odd(n - 1)
and is_odd n =
  match n with
    | 0 -> false
    | _ -> is_even(n - 1) 

(*3*)
let rec f_rec n =
  if n < 3 then n else f_rec(n - 1) + 2 * f_rec(n - 2) + 3 * f_rec(n - 3)

let rec f_iter_helper n c b a = 
  if n <= 0 then a else f_iter_helper (n - 1) (3 * a + 2 * b + c) c b

let f_iter n = f_iter_helper n 2 1 0

(*4*)
let rec pascal_coefficient r i = 
  match r, i with
    | r', _ when r' < 1 -> failwith "invalid arguments"
    | _, c' when c' < 1 -> failwith "invalid arguments"
    | _, _ when i > r -> failwith "invalid arguments"
    | 1, _ -> 1
    | _, 1 -> 1
    | _, _ when r = i -> 1
    | _, _ -> (pascal_coefficient (r - 1) (i - 1))  + (pascal_coefficient (r - 1) i)