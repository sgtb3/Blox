type primi_typ =        (* Primitive type constructors - Block should probably be here *)
    Int of int          (* A primitive integer takes 1 arg: an int *)
  | Void                (* A void primitive takes no args *)

type expr =             (* Expression constructors (of arg1 * arg2 * ... ) *)
    Int of int          (* An integer expression takes 1 arg: an int *)
  | Id of string        (* An Id expression takes 1 arg: a string *)

type stmt =             (* Statement constructors (of arg1 * arg2 * ... ) *)
    Block of stmt list  (* A block statement takes 1 arg: a list of statements *)
  | Expr of expr        (* An expression-statement takes 1 arg: an expression *)

type fr_decl = {        (* Frame declaration *)
  x : int;              (* 3 ints representing dimensions *)
  y : int;
  z : int; 
  fr_name : string;     (* A string representing the frame name *)
}

type program =          (* A Blox program *)
  fr_decl list          (* A list of frame declarations *)

(* 
  FYI:
  - Order for types matters. Any type used in another type must be declared 
    earlier. Also, it helps to start from the bottom and trace backwards. 
  - Comments throw off the line numbers shown in the error messages, so I 
    put all the comments on the same line as code for all files.
*)