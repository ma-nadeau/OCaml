(* GRADE:  100% *)
(* Question 1: Tree Depth *)
(* TODO: Write a good set of tests for testing your tree depth function. *)
(* For the whole assignment, you only need to write tests for `int tree`s
   etc. However, your functions must still be suitably polymorphic, and the
   grader will check this. *)
let tree_depth_cps_test_cases : (int tree * int) list = [ 
  (Empty, 0); 
  (Tree (Empty, 1, Empty), 1);
  (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), 2); 
  (Tree (Empty, 1, Tree (Empty, 2, Tree (Empty, 3, Tree (Empty, 4, Empty)))), 4); 
];;

(* These are the test cases that will actually be graded, but
   you don't have to modify this. Remember that you only need
   to test with the `id` continuation. `insert_test_continuations`
   (defined in the prelude) adds the `id` continuation to each of
   your test cases. *)
let tree_depth_cps_tests : ((int tree * (int -> int)) * int) list =
  insert_test_continuations tree_depth_cps_test_cases

(* An example of Non-CPS function to find depth of a tree: *)
let rec tree_depth (t : 'a tree) =
  match t with
  | Empty -> 0
  | Tree (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

(* TODO: Implement a CPS style tree_depth_cps function.*)
let rec tree_depth_cps (t : 'a tree) (return : int -> 'r) : 'r = 
  match t with
  | Empty -> return 0
  | Tree (l, _, r) -> tree_depth_cps l ( 
      fun ys -> 
        tree_depth_cps r (
          fun ks ->
            return ( 
              if ys >= ks then 1+ys
              else 1+ks)))
      

(* Question 2: Tree Traversal *)
(* TODO: Write a good set of tests for testing your tree traversal function. *)
let traverse_cps_test_cases : (int tree * int list) list = [ 
  (Tree (Empty, 42, Empty), [42]);  

  
  (Tree (Tree (Tree (Empty, 3, Empty), 2, Empty), 1, Empty), [1; 2; 3]);
  
  (Tree (Empty, 1, Tree (Empty, 2, Tree (Empty, 3, Empty))), [1; 2; 3]);

  (Tree (Tree (Tree (Empty, 4, Empty), 2, Tree (Empty, 5, Empty)), 
         1, 
         Tree (Tree (Empty, 6, Empty), 3, Tree (Empty, 7, Empty))),
   [1; 2; 4; 5; 3; 6; 7]);

  
  (Tree (Tree (Tree (Tree (Empty, 8, Empty), 4, Empty), 2, Tree (Empty, 5, Empty)), 
         1, 
         Tree (Empty, 3, Tree (Tree (Empty, 6, Empty), 7, Tree (Empty, 9, Empty)))),
   [1; 2; 4; 8; 5; 3; 7; 6; 9]);
  
];;

let traverse_cps_tests : ((int tree * (int list -> int list)) * int list) list =
  insert_test_continuations traverse_cps_test_cases

(* An example of non-CPS function to preorder traverse a tree *)
let rec tree_traverse (t : 'a tree) = 
  match t with
  | Empty -> []
  | Tree (l, x, r) -> x :: tree_traverse l @ tree_traverse r

(* TODO: Implement a CPS style preorder traversal function. *)
let rec traverse_cps (t : 'a tree) (return : 'a list -> 'r) : 'r = 
  match t with
  | Empty -> return []
  | Tree (l, x, r) -> 
      traverse_cps l (
        fun d1 -> 
          traverse_cps r (
            fun d2 ->
              return ( x :: d1 @ d2 )))
              

(* Question 3: Max Elements in a Tree *)
(* TODO: Write a good set of tests for testing your tree maximum function. *)
let tree_max_cps_test_cases : (int tree * int) list = [
  (Tree (Empty, 42, Empty), 42);
  (Tree (Tree (Tree (Empty, 3, Empty), 2, Empty), 1, Empty), 3);
  (Tree (Empty, 1, Tree (Empty, 2, Tree (Empty, 3, Empty))), 3);
  (Tree (Tree (Tree (Empty, 4, Empty), 2, Tree (Empty, 5, Empty)), 
         1, 
         Tree (Tree (Empty, 6, Empty), 3, Tree (Empty, 7, Empty))),
   7);
  (Tree (Tree (Tree (Tree (Empty, 8, Empty), 4, Empty), 2, Tree (Empty, 5, Empty)), 
         1, 
         Tree (Empty, 3, Tree (Tree (Empty, 6, Empty), 7, Tree (Empty, 9, Empty)))),
   9);
  (Empty, -1);
];;
let tree_max_cps_tests : ((int tree * (int -> int)) * int) list =
  insert_test_continuations tree_max_cps_test_cases

(* TODO: Implement a CPS style tree maximum function. *)
let rec tree_max_cps (t : int tree) (return : int -> 'r) : 'r = 
  match t with
  | Empty -> return (-1)
  | Tree (l,x,r) ->
      tree_max_cps l (
        fun d1 -> tree_max_cps r (
            fun d2 -> 
              maxk x (if d1 >= d2 then d1 else d2) return))







