(* GRADE:  100% *)
(** Part 1: Parsing *)
let parse_exp_tests : (string * exp option) list = [
  ("-456", Some (PrimUop (Negate, ConstI 456)));
  ("true", Some (ConstB true));
  ("false", Some (ConstB false)); 
  ("xVar", Some (Var "xVar"));
  ("c + d", Some (PrimBop (Var "c", Plus, Var "d"))); 
]

let rec exp_parser i =
  let open Parser in
  (** Use [identifier] and [keyword] in your implementation,
      not [identifier_except] and [keyword_among] directly *)
  let identifier, keyword =
    let keywords = ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
    identifier_except keywords, keyword_among keywords
  in 
  
  let atomic_exp =
    first_of [
      (* map f p applies the function f to the result of p *)
      map ( fun i -> ConstI i) int_digits;           (* Integer constants *)
      const_map (ConstB true) (keyword "true");      (* Boolean constant true *)
      const_map (ConstB false) (keyword "false");    (* Boolean constant false *) 
      map (fun x -> Var x) identifier;              (* Variables *)
      map3 (fun _ expr _ -> expr) (symbol "(") exp_parser (symbol ")"); (* Parenthesized expression *)
    ]
  in
  (* Left associative function application *)
  let applicative_exp = 
    first_of [ 
      left_assoc_op (symbol "") atomic_exp (fun e1 _ e2 -> Apply (e1, e2));
      atomic_exp;
    ]
  in
  

  let parse_let_comma_binding () =
    between 
      (symbol "(") (symbol ")") 
      (map2 (fun x y -> (x, y)) 
         identifier 
         (symbol "," |>> identifier)) 
    |*> fun (x, y) ->
      map2 (fun e1 e2 -> LetComma (x, y, e1, e2))
        (symbol "=" |>> exp_parser)
        (keyword "in" |>> exp_parser)
      |*> fun let_comma -> keyword "end" |>> of_value let_comma
  in
  
  let parse_let_binding () =

    map3 (fun x e1 e2 -> Let (x, e1, e2))
      identifier
      (symbol "=" |>> exp_parser) 
      (keyword "in" |>> exp_parser |*> fun e2 ->
           keyword "end" |>> of_value e2)
  in
  
  let parse_if_then_else () = 
    
    map3 (fun cond then_branch else_branch -> If (cond, then_branch, else_branch))
      (keyword "if" |>> exp_parser) 
      (keyword "then" |>> exp_parser) 
      (keyword "else" |>> exp_parser) 
      
  in
  

  let parse_function () = 
    
    map3 ( fun x  y z -> Fn (x, y, z))
      (keyword "fn" |>> identifier)
      (optional (symbol ":" |>> typ_parser))
      (symbol "=>" |>> exp_parser) 
      
  in
  

  let parse_recursion () = 
    
    map3 ( fun x y z -> Rec (x, y, z))
      (keyword "rec" |>> identifier)
      (optional (symbol ":" |>> typ_parser))
      (symbol "=>" |>> exp_parser)
      
  in
  
  let negatable_exp =
    first_of [
      (keyword "let" |*> fun _ ->
          first_of [
            parse_let_comma_binding ();
            parse_let_binding ();
          ]); 
      (* If-then-else expression *)
      parse_if_then_else (); 
      (* Function with optional type annotation *)
      parse_function (); 
      (* Recursion with optional type annotation *)
      parse_recursion ();
      (* | applicative_exp *) 
      applicative_exp;
    ]
  in
  
  (* Prefix unary operation - *)
  let negation_exp = 
    (* 
      ["-"] negatable_exp (* Prefix unary operation - *) 
    *) 
    prefix_op (const_map Negate (symbol "-")) negatable_exp (fun op e -> PrimUop (op, e)); 
                             
  in

  (* Left associative binary op * *)
  let multiplicative_exp =
   
    first_of [ 
      (* 
        | multiplicative_exp "*" negation_exp (* Left associative binary op * *)
      *)
      left_assoc_op (const_map Times (symbol "*")) negation_exp (fun e1 op e2 -> PrimBop (e1, op, e2));
      (* 
        | negation_exp
      *)
      negation_exp
    ]
  in

  let additive_exp =
    
    first_of [
      (*
      | additive_exp "+" multiplicative_exp (* Left associative binary op + *)
      | additive_exp "-" multiplicative_exp (* Left associative binary op - *) 
      *)
      left_assoc_op (
        first_of [ 
          const_map Minus (symbol "-");
          const_map Plus (symbol "+");
        ]) 
        multiplicative_exp  (fun e1 op e2 -> PrimBop (e1, op, e2));
      (*
      | multiplicative_exp
      *)
      multiplicative_exp;
    ] 
  in

  let comparative_exp =
    first_of [
      (* 
      | additive_exp "=" additive_exp (* Non-associative binary op = *)
      | additive_exp "<" additive_exp (* Non-associative binary op < *) 
      *)
      non_assoc_op (
        first_of [
          const_map LessThan (symbol "<"); (* Non-associative binary op < *)
          const_map Equals (symbol "="); (* Non-associative binary op = *) 
        ])
        additive_exp 
        (fun e1 op e2 -> PrimBop (e1, op, e2)); 
      (* 
      | additive_exp
      *)
      additive_exp;
    ]
  in


  let exp_parser =
    
    first_of [
      (* 
      comparative_exp "," comparative_exp
      (* Non-associative binary operation pair construction (comma) *) 
      *)
      non_assoc_op (symbol ",") comparative_exp (fun e1 _ e2 -> Comma (e1, e2));
      (*
      | comparative_exp
      *)
      comparative_exp;
    ]
  in

  exp_parser i
  
  

(** DO NOT Change This Definition *)
let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)

