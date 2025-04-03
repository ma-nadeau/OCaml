(* GRADE:  100% *)
(* SECTION 1 *)

(*  Question 1.1 *)
let rec repeat (x : 'a) : 'a stream = 
  { head = x; 
    tail = Susp (fun () -> repeat x) 
  }


(* Question 1.2 *)
let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  if f s.head then
    { head = s.head; 
      tail = Susp ( fun () -> filter f (force s.tail)) }
  else
    filter f ( force s.tail)

(* Question 1.3 *)
let rec lucas1 =
  {
    (* You should fix these *)
    head = 2;
    tail = Susp (fun () -> lucas2);
  }

and lucas2 =
  {
    (* You should fix these *)
    head = 1;
    tail = Susp (fun () -> zip_with (+) lucas1 lucas2 );
  }

(* Question 1.4 *)
let unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream =
  let rec generate (seed : 'a) : ('b * 'a) stream =
    { head = f seed;
      tail = Susp ( fun () -> generate ( snd (f seed)))
    }
  in 
  map (fun (x, _) -> x) (generate seed)
    

(* Question 1.5 *)
let lucas_step (a, b) = (a, (b, a + b))
                        
let unfold_lucas : int stream = unfold lucas_step (2, 1)

(* SECTION 2 *)

let scale (s: 'a stream) (factor: int) : int stream =
  map (fun x -> x * factor) s

let rec s = 
  { head = 1;
    tail = Susp (fun () -> 
        let two = scale s 2 in
        let three = scale s 3 in
        let five = scale s 5 in
        let rest =  merge two three in
        merge rest five) 
  }
  

