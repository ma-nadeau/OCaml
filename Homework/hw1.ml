(* GRADE:  100% *)
(* Question 1: Manhattan Distance *)
(* TODO: Write a good set of tests for distance. *)
let distance_tests = [
  (
    ((0, 0), (0, 0)), (* input: two inputs, each a pair, so we have a pair of pairs *)
    0                 (* output: the distance between (0,0) and (0,0) is 0 *)
  );                    (* end each case with a semicolon *)
    (* Your test cases go here *)
  
  (* distance a a = 0 the distance from a point to itself is always zero *)
  (
    ((7,7), (7,7)),
    0 
  );
  (
    ((3,3), (3,3)),
    0 
  );
  (
    ((1,1), (1,1)),
    0 
  );
  
  (* Horizontal distance *)
  (
    ((0, 0), (3, 0)), 
    3
  );
  
  (
    ((3, 0), (0, 0)), 
    3
  );

  (* Vertical distance *)
  (
    ((0, 0), (0, 4)), 
    4
  );
  
  (
    ((0, 4), (0, 0)),
    4
  );

  
  (* distances are always non-negative *) 
  (
    ((4,3), (7,7)),
    7
  );
  
  (
    ((-7,-7), (0,0)),
    14
  );
  
  (
    ((1,2), (-4,3)),
    6
  ); 
  
  (
    ((7,7), (4,3)),
    7
  );
  
  (
    ((0,0), (-7,-7)),
    14
  );
  
  (
    ((-4,3),(1,2)),
    6
  ); 
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance (x1, y1) (x2, y2) = 
  abs (x1 - x2) + abs (y1 - y2) 

;;


(* Question 2: Binomial *)
(* TODO: Write your own tests for the binomial function.
         See the provided test for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct this incorrect test case for the function. *)
  
  (
    (0, 0), 
    1
  );  
  (
    (1, 0), 
    1
  ); 
  (
    (1, 1), 
    1
  ); 
  (
    (4, 2), 
    6
  );
  (
    (5, 3), 
    10
  );
  (
    (6, 0),
    1
  );
  (
    (6, 6),
    1
  ); 
  (
    (7, 3),
    35
  );
  (
    (10, 5),
    252
  );
  (
    (25, 11),
    54
  );
  (
    (25,11),
    54
  );
  (
    (19,11),
    0
  );
  (
    (16,11),
    4
  );
  (
    (13,11),
    24
  );
  (
    (31,11),
    0
  );
  (
    (28,11),
    0
  );
  (
    (23,11),
    4
  ) 
]

(* TODO: Correct this implementation so that it compiles and returns
the correct answers.
*)
let binomial n k =
  let rec factorial value =
    if value = 0 then 1
    else value * factorial (value - 1) 
  in
  (* B(n, k) = n! / (k! (n - k)!) *) 
  factorial n / ( factorial k * factorial (n - k) )
  
           



(* Question 3: Lucas Numbers *)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
  (0, 2);   
  (1, 1);   
  (2, 3);   
  (3, 4);   
  (4, 7);   
  (5, 11);  
  (6, 18);  
  (10, 123)
  
]

(* TODO: Implement a tail-recursive helper lucas_helper. *)
let rec lucas_helper ( n, a, b) = 
  if n = 0 then a 
  else if n = 1 then b
  else lucas_helper ( n-1, b, a+b )
  


(* TODO: Implement lucas by calling lucas_helper. *) 
let lucas n = 
  lucas_helper(n, 2, 1)
