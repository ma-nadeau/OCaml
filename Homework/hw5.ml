(* GRADE:  94% *)



(* TODO: Add test cases for both Question 1 and 2. *)
let xyz_truth_asgn : truth_assignment =
  Variable_map.empty
  |> Variable_map.add "x" true
  |> Variable_map.add "y" false
  |> Variable_map.add "z" false

let ab_truth_asgn : truth_assignment =
  Variable_map.empty
  |> Variable_map.add "a" true
  |> Variable_map.add "b" true

let x_truth_asgn : truth_assignment =
  Variable_map.empty
  |> Variable_map.add "x" true

let x_false_y_true_truth_asgn : truth_assignment =
  Variable_map.empty
  |> Variable_map.add "x" false
  |> Variable_map.add "y" true
    
let xyz_truth_asgn : truth_assignment = 
  Variable_map.empty
  |> Variable_map.add "x" true
  |> Variable_map.add "y" true
  |> Variable_map.add "z" false
    
    
let find_sat_assignment_tests : (formula * truth_assignment option) list = [
  (parse_formula "x & ~x", None);
  (parse_formula "x | y", Some xyz_truth_asgn);
  (parse_formula "a & b", Some ab_truth_asgn);
  (parse_formula "x | ~x", Some x_truth_asgn);
  (parse_formula "p & (~p & q)", None); 
  (parse_formula "(x & y) | ~x", Some x_false_y_true_truth_asgn);
  (parse_formula "(x & (y | z)) | (~x & ~(y & z))", Some xyz_truth_asgn);
  


]

(* Question 1 *)
(*----------------------------------------*)

let rec find_assignment (vars : string list) (assignment : truth_assignment)
    (formula : formula) : truth_assignment =
  match vars with
  | [] -> (* empty list *)
      if eval assignment formula then assignment (* if evaluates the formula
                                                 to true, then we return the
                                                 assingment*)
      else raise Unsatisfiable_formula (* otherwise we raise error *) 
  | v :: vs -> (* non-empty list, v first element, vs remaining list of var *) 
      try find_assignment vs (Variable_map.add v true assignment) formula 
            (* tries to assign true to v by updating the assignment
   using Variable_map.add v true assignment *)
(* Then, if recursively call find_assignment *)
      with Unsatisfiable_formula -> 
        find_assignment vs (Variable_map.add v false assignment) formula
(* if recursive call didn't work (i.e. Unsatisfiable_formula), 
variable should've been false*)
          
(* TODO: Implement the function. *)
let find_sat_assignment_exc (formula : formula) : truth_assignment =
  let vars = collect_variables formula in
  find_assignment vars Variable_map.empty formula
  
(* Question 2 *)
(*----------------------------------------*)

(* TODO: Implement the function. *)
let rec find_sat_assignment_cps (formula : formula)
    (return : truth_assignment -> 'r) (fail : unit -> 'r) : 'r =
  
  let vars = collect_variables formula in
  
  let rec try_assignments vars assignment return fail = 
    match vars with
    | [] -> 
        if eval assignment formula then return assignment 
        else fail ()  
    | v :: vs -> 
        try_assignments vs  (Variable_map.add v true assignment) return 
          (fun () ->
             try_assignments vs (Variable_map.add v false assignment) return fail
          )
  in
  try_assignments vars Variable_map.empty return fail
    
    








