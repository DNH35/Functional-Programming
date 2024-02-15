let fib n = 
  let rec iter a b n = 
    if n = 0 
      then a
  else iter b (a + b) (n - 1) 
in 
  iter 0 1 n