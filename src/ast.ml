(* FYI - order or types matters. Any type used in another type must be declared earlier. Also, it helps to start from the bottom and trace backwards. *)

(* Primitive type constructors - Block should probably be here *)
type primi_typ = 
    Int of int          (* A primitive integer takes 1 arg: an int *)
  | Void                (* A void primitive takes no args *)

(* Expression constructors (of arg1 * arg2 * ... ) *)
type expr = 
    Int of int          (* An integer expression takes 1 arg: an int *)
  | Id of string        (* An Id expression takes 1 arg: a string *)

(* Statement constructors (of arg1 * arg2 * ... ) *)
type stmt = 
    Block of stmt list  (* A block statement takes 1 arg: a list of statements *)
  | Expr of expr        (* An expression-statement takes 1 arg: an expression *)

(* Frame declaration *)
type fr_decl = { 
  x : int;              (* 3 ints representing dimensions *)
  y : int;
  z : int; 
  fr_name : string;     (* A string representing the frame name *)
}

(* A Blox program *)
type program = 
  fr_decl list (* A list of frame declarations *)
