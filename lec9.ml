(* type complex = C of float * float *)
type complex = {real : float; imag : float}

let complex_add_lists c1 c2 =
  match (c1, c2) with
  | ([r1; i1], [r2; i2]) -> [r1 +. r2; i1 +. i2]
  | _ -> failwith "bad complex numbers"

let complex_add_tuples (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)

(* let complex_add_adt (C (r1, i1)) (C (r2, i2)) = C (r1 +. r2, i1 +. i2) *)

let complex_add_records c1 c2= 
  {real = c1.real +. c2.real ;
  imag = c1.imag +. c2.imag};;
 
