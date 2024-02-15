let rec sum_i_1_n n =
  if n = 1
    then 1
else 
  n + sum_i_1_n (n - 1)

let rec sum_i2_0_n n =
    if n = 0
      then 0
  else 
    n * n + sum_i2_0_n (n - 1)

let rec sum_3i3_m_n m n =
  if m > n then
    0
  else 
    3 * m * m * m + sum_3i3_m_n (m + 1) n


let rec rsum f m n =
  if m > n then
    0
  else 
    f m + rsum f (m + 1) n

let sum_i_1_n' n = 
    rsum (fun m -> m * m) 1 n    

let rec sum_step f step m n =
    if m > n then
       0
    else 
      f m + sum_step f step (m + step) n

let rec gsum f next lo hi =
  if lo > hi then
    0
 else 
   f lo + gsum next f (next lo) hi

let sum f lo hi =
  gsum f (fun n -> n + 1) lo hi

let sum2 f lo hi =
  gsum f (fun n -> n * 2) lo hi

let rec fgsum f next lo hi =
  if lo > hi
    then 0.0
else 
  f lo +. fgsum f next (next lo) hi

let integral f lo hi dx =
  dx *. fgsum f (fun x -> x +. dx) lo hi