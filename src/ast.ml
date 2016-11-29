type primi_typ =        (* Primitive type constructors - Block should probably be here *)
    Int of int          (* A primitive integer takes 1 arg: an int *)
  | Void                (* A void primitive takes no args *)

type expr =             (* Expression constructors (of arg1 * arg2 * ... ) *)
    Int of int          (* An integer expression takes 1 arg: an int *)
  | Id of string        (* An Id expression takes 1 arg: a string *)
  | Assign of string * expr

type face_id = {
  x : int;
  y : int;
  z : int;
  face : string;
}

type join_arg = {
  fr_name : string;
  blck_face : face_id list;  (*This should technically be a set of faces, not just one necessarily *)
}

type join = {
  fr_a : join_arg;
  fr_b : join_arg;
}

type fr_decl = {        (* Frame declaration *)
  x : int;              (* 3 ints representing dimensions *)
  y : int;
  z : int;
  fr_name : string;     (* A string representing the frame name *)
}

type fr_print = {
  fr_id : string;
}

type stmt =             (* Statement constructors (of arg1 * arg2 * ... ) *)
    Block of stmt list  (* A block statement takes 1 arg: a list of statements *)
  | Expr of expr        (* An expression-statement takes 1 arg: an expression *)
  | Join of join_arg * join_arg
  | Fr_decl of int * int * int * string
  | Fr_print of string

type program =            (* A Blox program *)
  stmt list               (* A list of stmts *)

let rec string_of_expr = function         (* print expressions *)
    Int(x) -> string_of_int x
  | Id(x)  -> x
  | Assign(x, y) -> "Frame " ^ x ^ " = " ^ string_of_expr y

let string_of_frdecl x y z name =                 (* print frame declarations *)
  "Frame<" ^ string_of_int x ^ "," ^ 
             string_of_int y ^ "," ^ 
             string_of_int z ^ "> " ^ 
             name ^ ";\n"

let string_of_face_id f =
  "(" ^ string_of_int f.x ^ ", " ^ string_of_int f.y ^ ", " ^ string_of_int f.z ^ ", " ^ f.face ^ ")"

let rec string_of_face_id_list = function
    face_id               -> string_of_face_id face_id
  | face_id face_id_list  -> string_of_face_id face_id ^ ", " ^ string_of_face_id_list face_id_list

let string_of_join_arg x =
  x.fr_name ^ ", {" ^ string_of_face_id_list x.blck_face ^ "}\n"


let rec string_of_stmt = function         (* print statements *)
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)   -> string_of_expr expr ^ "\n"
  | Join(x, y)   -> "Join(" ^ string_of_join_arg x ^ ", " ^ string_of_join_arg y ^ ")\n"
  | Fr_decl(x, y, z, name) -> string_of_frdecl x y z name
  | Fr_print(name) -> "Print " ^ name ^ "\n"

let string_of_program s =            (* print program (a list of frame declarations) *)
  String.concat "" (List.rev (List.map string_of_stmt s))

(* 
  FYI:
  - Order for types matters. Any type used in another type must be declared 
    earlier. Also, it helps to start from the bottom and trace backwards. 
  - Comments throw off the line numbers shown in the error messages, so I 
    put all the comments on the same line as code for all files.
*)