(* GRADE:  100% *)
let open_account (pass : password) : bank_account =
  
  (* ref -> creates a mutable refrence to a value *)
  (* := -> used to update the value stored in a reference *)
  (* ! -> used to access the current value of a reference. *)
  
  
  (* TODO: create any helper variables and/or functions *)
  let balance = ref 0 in
  let attempts = ref 0 in
  let locked = ref false in
  
  let check_password p =
    if !locked then raise account_locked
    else if p <> pass then (
      attempts :=  !attempts + 1; 
      if !attempts >= 3 then locked := true;
      raise wrong_pass 
    ) else attempts :=0
  in
  
  let deposit p amt =
    check_password p;
    if amt < 0 then raise negative_amount
    else balance := !balance + amt
  in
  
  

  (* TODO: Implement deposit to add money to the account *)
  let deposit p amt = 
    check_password p;
    if amt < 0 then raise negative_amount
    else balance := !balance + amt
  in
  
  
  (* TODO: Implement show_balance for the account *)
  let show_balance p = 
    check_password p;
    !balance
  in
  
  (* TODO: Implement withdraw money for the account *)
  let withdraw p amt = 
    check_password p;
    if amt < 0 then raise negative_amount
    else if amt > !balance then raise not_enough_balance
    else balance := !balance - amt
  in
  
  {
    deposit;
    show_balance;
    withdraw;
  }
