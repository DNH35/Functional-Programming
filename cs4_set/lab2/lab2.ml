(**)

open Num

let ni = num_of_int     (* convert int -> num *)

(*
Part A:

A.0. 
Time complexity: O(log(n))
The function calls itself to continously divides n by 2 until n becomes an odd number > 1 or 1,
so this function has log time complexity because the number of calls is logarithmically proportional
to n.  

Space complexity: O(1)
This is a tail recursion so the OCaml, which OCaml compilers cann apply tail recursion optimization and
use constant stack space. So the function has a constant space complexity


A.1.
Space complexity: O(N)
Consider fib(7) in applicative order evaluation:
fib(7) will be evaluated to fib(6) + fib(5)
Current stack: fib(7)
fib(6) will be evaluated first:
fib(6) = fib(5) + fib(4)
Current stack: fib(7), fib(6)
fib(5) will be evaluated first:
fib(5) = fib(4) + fib(3)
Current stack: fib(7), fib(6), fib(5)
fib(4) will be evaluated first:
fib(4) = fib(3) + fib(2)
Current stack: fib(7), fib(6), fib(5), fib(4)
fib(3) will be evaluated first:
fib(3) = fib(2) + fib(1)
Current stack: fib(7), fib(6), fib(5), fib(4), fib(3)
fib(2) will be evaluated to 2, and fib(1) will be evaluated to 1 and the compiler will start evaluate
all pending operations from here (fib(4), fib(5), fib(6), fib(7))
Thus, we can see that the number of elements can go up to fib(7), fib(6), fib(5), fib(4), fib(3),
so we can conclude that the space complexity is O(N) because the number of call is linearly proportional
to the number of element, approximately. 

A.2. 

1.
  When sine 12.5 is called, the function will be evaluated as follow:
  sine 12.5 is evaluated to:
    12.5 > 0.1, so sin 12.5 -> p(sin (12.5 / 3))
  12.5 / 3 ~ 4.1667
  sine 4.1667 will be evaluated first, p(sin 4.1667) becomes pending operation
  sine 4.1667 is evaluated to:
    4.1667 > 0.1, so sin 4.1667 -> p(sin (4.1667/ 3))
  4.1667 / 3 ~ 1.3889
  sine 1.3889 is evaluated to:
    1.3889 > 0.1, so sin 1.3889 -> p(sin (1.3889/ 3))
  1.3889 / 3 ~ 0.463
  sine 0.463 is evaluated to:
    0.463 > 0.1, so sin 0.463 -> p(sin (0.463/ 3))
  0.463 / 3 ~ 0.15
  sine 0.15 is evaluated to:
    0.15 > 0.1, so sin 0.15 -> p(sin (0.15/ 3))
  0.15 / 3 ~ 0.05
  sine 0.05 is evaluated to:
    0.05 < 0.1, so sin 0.05 -> 0.05
  Now the compilers will take care of all the pending operations, notice that it will evaluate p 5 times.
  Thus, p will be applied 5 times when sine 12.5 is called

  2.
    Order of growth in steps: O(log(a))
    Order of growth in space: O(log(a))

    The order of growth in steps is O(log(a)) because the function continously divides a by 3 until a < 0.01
    and the function is evaluated to a. Consider part 1 of this question, sine calls itself 5 times for
    sine 12.5, and 2 * log_3(12.5) + 1 is approximately equal to 5 calls. Since we don't care about constant factor and
    the + 1 for big O, we can conclude the growth in steps is O(log(a)). Also from part 1 of the question, we can see that
    the pending operation and the size of the stack state is proportional to the number of calls to sine, so the order of growth
    in space is also O(log(a))


*)

(*
A.3.  
*)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
      | 0 -> 1
      | _ when is_even n -> square (fast_expt b (n / 2))
      | _ -> b * fast_expt b (n - 1)

let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter a b n =
    match n with
      | 0 -> a
      | _ when is_even n -> iter a (square b) (n / 2)
      | _ -> iter (a * b) b (n - 1)
  in 
  iter 1 b n

(*
A.4.  
*)
let fast_mult a b =
  let double m = m + m in
  let rec halve m = 
    match m with
      | 0 -> 0
      | _ -> 1 + halve (m - 2)
  in
  let rec fast_mult_helper a b =
    if a = 0 
      then 0
  else 
    match b with
      | 0 -> 0
      | _ when b mod 2 = 0 -> fast_mult_helper (double a) (halve b)
      | _ -> a + fast_mult_helper a (b - 1)
in
fast_mult_helper a b

(*A.5*)
let ifast_mult a b =
  let double m = m + m in
  let rec halve m = 
    match m with
      | 0 -> 0
      | _ -> 1 + halve (m - 2)
  in
  let rec ifast_mult acc a b =
    match b with
      | 0 -> acc
      | _ when b mod 2 = 0 -> ifast_mult acc (double a) (halve b)
      | _ -> ifast_mult (acc + a) a (b - 1)
in
ifast_mult 0 a b

(*
A.6.
  Time Complexity: O(n)
  Space Complexity: O(log(n))

    At each recursive call, the function divdes n by two, and foo is making a recursive call and halving n twice, which means
    this has a binary tree recusive calls structure. Now recall that tree recursion has exponential time complexityj, so at
    each level of the recursive tree, the function calls are exponentially increased, leading to the time complexity of O(2^(logn)),
    which simplifies to having O(n). 
    
    Each time foo calls itself when n > 1, there's pending operaiton and the stack state can go up to log_2(n) + 1 
    (for example, for foo 4, the stack statevcontains at most foo 4, foo 2, foo 1). Big O doesn't care about 
    constant factor, so the space complexity is O(log(n)) Notifce that because f is evaluated at constant time, 
    so it doesn't affect the time and space complexity

A.7.
  1. The function is linear iterative recursion because fib n has an internal recursive method last_two that has
  an iterative-like structure by accumlating values through recursive calls without the need of extra space. 
  2. Time complexity is O(n) because the number of recursive calls is proportional to n, and each call takes constant time. 
     Space complexity is O(n) because at each recursive calls, there's pending operations on the addition, and the function
     is decrementing n as it calls itself, so the number of recursive calls stored in the stack is proportional to n, leading to
     the space complexity to be O(n). 
*)

(*
Part B
*)

(*

B.1

a. (fun x y -> x * (2 + y)) 20 (2 * 4)   
b. (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
c. (fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1
d. (fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
   (fun x -> (x * x * x) 3) (*shielding*)


B.2

let x = 2 * 10
and y = 3 + 4
in
  let y = 14 in
  let z = 22 in
    x * y * z

Desugar:

(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)

Evaluate x = 2 * 10
  evaluate 2 -> 2
  evaluate 10 -> 10
  evaluate * -> primitive function *
  apply * to 2, 10 -> 20
  evaluate x to 20

Evaluate y = 3 + 4
  evaluate 3 -> 3
  evaluate 4 -> 4
  evaluate + -> primitive function +
  apply + to 3, 4 -> 7
  evaluate y to 7

Substitute these values to 
  let y = 14 in
    let z = 22 in
      x * y * z

Inner y is shielded, so y is evaluated to 14
Evaluate z to 22

Substitute these values to x * y * z
  (x * y) * z
    evaluate x to 20
    evaluate y to 14 (lambda shielding)
    evaluate * to primitive function
    apply * to 20, 14 -> 280
  
  substitute this value for (x * y)
    evaluate (x * y) to 280
    evaluate z to 22
    apply * to 280, 22 to 6160

Result = 6160

B.3

(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

The reason for x being unbound is because when y is evaluated to x * 2, the x in x * 2 is different from the x in x = 10
as we can see from desugaring the let expression. x * 2 is outside of the scope of (fun x y z ...).
F
Fix:
let x = 10 in
let y = x * 2 in
let z = y + 3
in x + y + z
*)

(*Part C*)
(*C.1*)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (result +/ term a)
  in
    iter a (ni 0)

(*C.2*)
let rec product_rec_helper term a next b =
  if a >/ b
    then (ni 1)
  else term a */ (product_rec_helper term (next a) next b)

let product_rec term a next b =
  product_rec_helper term a next b

let factorial n = n

let factorial_rec n = 
  product_rec factorial (ni 1) (fun n -> n +/ (ni 1)) n

let product_iter term a next b =
  let rec product_iter_helper a result =
    if a >/ b
      then result
  else product_iter_helper (next a) (result */ term a) 
in
product_iter_helper a (ni 1)

let factorial_iter n =
  product_iter factorial (ni 1) (fun n -> n +/ (ni 1)) n

let pi_term n = 
  ((ni 2) */ n) // ((ni 2) */ n -/ (ni 1)) */ ((ni 2) */ n) // ((ni 2) */ n +/ (ni 1))
let pi_product n = 
    product_iter pi_term (ni 1) (fun n -> n +/ (ni 1)) n
let pi_approx = float_of_num ((ni 2) */ pi_product (ni 1000))  (* defined in terms of pi_product *)

(*C.3*)
let rec accumulate_rec combiner null_value term a next b =
  if a >/ b
    then null_value
else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

let accumulate_iter combiner null_value term a next b =
  let rec accumulate_iter_helper a result =
    if a >/ b
      then result
  else accumulate_iter_helper (next a) (combiner (term a) result)
in
accumulate_iter_helper a null_value

let sum term a next b =
  accumulate_iter (+/) (ni 0) term a next b

let product term a next b =
  accumulate_iter ( */ ) (ni 1) term a next b

(*C.4*)
let compose f g x = f (g x)

(*C.5*)
let rec repeated f n = 
  match n with
    | 1 -> f
    | _ -> compose f (repeated f (n - 1))

(*C.6*)
let smooth dx f = 
  fun x -> (f (x -. dx) +. f(x) +. f(x +. dx)) /. 3.0

let rec nsmoothed dx f n =
 match n with
  | 0 -> f
  | _ -> nsmoothed dx (smooth dx f) (n - 1)

(*Part D*)

(*D.1*)
let is_prime n =
  if n <= 1
    then false
else 
   let rec is_prime_helper n i =
    match i with
      | 1 -> true
      | _ when n = 2 -> true
      | _ when n mod i = 0 -> false
      | _ -> is_prime_helper n (i - 1) 
   in
   is_prime_helper n (int_of_float(sqrt (float_of_int(n))) + 1)

(*D.2*)
let smallest_prime_factor n = 
  if n < 2 || is_prime n
    then invalid_arg "Invalid argument, n < 2 or n is prime"
else 
  let rec smallest_prime_factor_helper i =
    if n mod i = 0 && is_prime i then
      i
    else
      smallest_prime_factor_helper (i + 1)
  in
  smallest_prime_factor_helper 2
  