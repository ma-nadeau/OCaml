(* GRADE:  100% *)
(* Hi everyone. All of these problems are generally "one-liners" and have slick solutions. They're quite cute to think
   about but are certainly confusing without the appropriate time and experience that you devote towards reasoning about
   this style. Good luck! :-) *)

(* For example, if you wanted to use the encoding of five in your test cases, you could define: *)
let zero : int church = fun s z -> z
let one : int church = fun s z -> s z
let two : int church = fun s z -> s (s z)
let three : int church = fun s z -> s (s (s z))
let four : int church = fun s z -> s (s (s (s z)))

let five : 'b church = fun s z -> s (s (s (s (s z)))) 
(* and use 'five' like a constant. You could also just use
   'fun z s -> s (s (s (s (s z))))' directly in the test cases too. *)

(* If you define a personal helper function like int_to_church, use it for your test cases, and see things break, you should
   suspect it and consider hard coding the input cases instead *)

(*---------------------------------------------------------------*)
(* QUESTION 1 *)

(* Question 1a: Church numeral to integer *)
(* TODO: Test cases *)
let to_int_tests : (int church * int) list = [
  (zero, 0);
  (one,  1);
  (two,  2);
  (three,3);
  (four, 4);
  (five, 5);
]

(* TODO: Implement:
   Although the input n is of type int church, please do not be confused. This is due to typechecking reasons, and for
   your purposes, you could pretend n is of type 'b church just like in the other problems. *)
let to_int (n : int church) : int = 
  n (fun x -> x + 1) 0

    
let zero0 : 'a church = fun f x -> x
let one1 : 'a church = fun f x -> f x
let two2 : 'a church = fun f x -> f (f x)
let three3 : 'a church = fun f x -> f (f (f x))
let four4 : 'a church = fun f x -> f (f (f (f x)))
    
(* Question 1b: Determine if a church numeral is zero *)
(* TODO: Test cases *)
let is_zero_tests : ('b church * bool) list = [ 
  (zero0, true);
  (one1, false);
  (two2, false);
  (three3, false); 
  (four4, false); 
]

(* TODO: Implement *)
let is_zero (n : 'b church) : bool =
  n (fun _ -> false) true

(* Question 1c: Add two church numerals *)
(* TODO: Test cases *)
let add_tests : (('b church * 'b church) * 'b church) list = [
  ((zero, zero), zero);
  ((zero, one), one);
  ((one, zero), one);
  ((one, one), two);
  ((two, one), three); 
]

(* TODO: Implement *)
let add (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun f x -> n1 f ( n2 f x )
  

(*---------------------------------------------------------------*)
(* QUESTION 2 *)

(* Question 2a: Multiply two church numerals *)
(* TODO: Test cases *)
let mult_tests : (('b church * 'b church) * 'b church) list = [
  ((zero, zero), zero);
  ((zero, one), zero);
  ((one, zero), zero);
  ((one, one), one);
  ((two, one), two); 
  ((two, two), four); 
]

(* TODO: Implement *)
let mult (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun f -> n2 (fun y -> n1 f y)


(* Question 2b: Compute the power of a church numeral given an int as the power *)
(* TODO: Test cases *)
let int_pow_church_tests : ((int * 'b church) * int) list = [
  ((1, zero), 1);
  ((0, one), 0);
  ((1, zero), 1);
  ((1, one), 1);
  ((2, one), 2); 
  ((2, two), 4); 
]

(* TODO: Implement *)
let int_pow_church (x : int) (n : 'b church) : int = 
  let exp = to_int n in
  let rec pow base exp =
    if exp = 0 then 1
    else base * (pow base (exp - 1))
  in
  pow x exp

