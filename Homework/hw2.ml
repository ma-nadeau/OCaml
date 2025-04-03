(* GRADE:  100% *)
(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = [
  (0, Z);
  (1, S Z);
  (2, S (S Z));
  (3, S (S (S Z)));
  (5, S (S (S (S (S Z))))); 
  
] 


(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let rec q1a_nat_of_int (n : int) : nat = 
  let rec helper (k : int) (acc: nat) : nat = 
    if k <= 0 then acc
    else helper (k-1) (S acc) 
  in
  helper n Z

(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = [ 
  (Z, 0);
  (S Z, 1);
  (S (S Z), 2);
  (S (S (S Z)), 3);
  (S (S (S (S (S Z)))), 5);
]

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let rec q1b_int_of_nat (n : nat) : int = 
  let rec helper2 (conv : nat) (acc : int) : int =
    match conv with 
    | Z -> acc
    | S conv -> helper2 conv (acc + 1)
  in 
  helper2 n 0

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [
  ((Z, Z), Z); 
  ((S Z, Z), S Z);
  ((S Z, S Z), S (S Z));
  ((S (S (S Z)), S (S (S (S (S Z))))), S ( S ( S ( S ( S ( S (S (S Z))))))));
  ((S (S (S (S (S Z)))), S (S (S (S (S Z))))),  S ( S ( S ( S ( S ( S ( S ( S (S (S Z)))))))))); 
]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat = 
  match m with
  | Z -> n
  | S conv -> q1c_add (S n) (conv) 
  

(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times ( Const (-1.0), e)

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = 
  Plus(e1, q2a_neg(e2))
    
(* TODO: Implement {!q2c_pow}. *)
let rec q2c_pow (e1 : exp) (p : nat) : exp = 
  match p with
  | Z -> Const 1.0
  | S n -> Times (e1, q2c_pow e1 n) 
  

(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = [
  ((3.0, Const 5.0), 5.0); 
  ((3.0, Var), 3.0);
  ((3.0, Plus (Const 2.0, Const 3.0)), 5.0);
  ((3.0, Times (Const 2.0, Const 3.0)), 6.0);
  ((3.0, Plus (Const 2.0, Times (Var, Const 3.0))), 11.0);
  ((2.0, Div (Times (Var, Const 4.0), Plus (Var, Const 2.0))), 2.0);
]
                                                

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = 
  match e with
  | Var -> a                        
  | Const c -> c
  | Plus (e1, e2) -> (eval a e1) +. (eval a e2)
  | Times (e1, e2) -> (eval a e1) *. (eval a e2)
  | Div (e1, e2) -> (eval a e1) /. (eval a e2)


(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = [
  (Const 5.0, Const 0.0);
  (Var, Const 1.0); 
  (Plus (Const 5.0, Var), Plus(Const 0.0, Const 1.0));
  (Times (Const 2.0, Var), Plus (Times (Const 0.0, Var), Times (Const 2.0, Const 1.0)));
  (Times (Const 2.0, Var), Plus (Times (Const 0.0, Var), Times (Const 2.0, Const 1.0)));
  (Div (Var, Const 2.0), Div (Plus (Times (Const 1.0, Const 2.0), Times (Const (-1.0), Times (Var, Const 0.0))), Times (Const 2.0, Const 2.0))); 
]

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = 
  match e with
  | Var -> Const 1.0
  | Const c -> Const 0.0
  | Plus (e1, e2) -> Plus (diff e1, diff e2)
  | Times (e1, e2) ->
      Plus ( 
        Times (diff e1, e2),
        Times (e1, diff e2)
      )
  | Div (e1, e2) -> Div (
      Plus (
        Times ( diff e1, e2),
        Times ( Const (-1.0), Times (e1, diff e2))
      ),
      Times (e2, e2)
    )
    
