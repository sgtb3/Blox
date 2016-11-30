type primi_typ =
    Int of int
  | Void

type expr =
    Int of int
  | Id of string
  | Assign of string * expr

type face_id = {
  dim  : int * int * int;
  face : string;
}

type join_arg = {
  frname : string;
  blck_face : face_id list;
}

type join = {
  fr_a : join_arg;
  fr_b : join_arg;
}

type fr_decl = {
  x : int;
  y : int;
  z : int;
  fr_name : string;
}

type fr_print = {
  fr_id : string;
}

type stmt =
    Block of stmt list
  | Expr of expr
  | Join of join_arg * join_arg
  | Fr_decl of int * int * int * string
  | Fr_print of string

type program = stmt list

(* print expressions *)
let rec string_of_expr = function         
    Int(x) -> string_of_int x
  | Id(x)  -> x
  | Assign(x, y) -> "Frame " ^ x ^ " = " ^ string_of_expr y ^ ";"

(* print frame declarations *)
let string_of_frdecl x y z name =         
  "Frame<" ^ string_of_int x ^ "," ^
             string_of_int y ^ "," ^
             string_of_int z ^ "> " ^
             name ^ ";\n"

(* print dimensions *)
let string_of_dim (x,y,z) =
  string_of_int x ^ "," ^
  string_of_int y ^ "," ^
  string_of_int z

(* print type face_id *)
let string_of_face_id f =
  "(" ^  string_of_dim f.dim ^ f.face ^ ")"

(* print list of face_id's *)
let string_of_face_id_list fid =
  String.concat "," (List.rev (List.map string_of_face_id fid))

(* print list of join args *)
let string_of_join_arg x =
  x.frname ^ ", {" ^ string_of_face_id_list x.blck_face ^ "}"

(* print statements *)
let rec string_of_stmt = function 
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)   -> string_of_expr expr ^ "\n"
  | Join(x, y)   -> "Join(" ^ string_of_join_arg x ^ ", " ^ string_of_join_arg y ^ ")\n"
  | Fr_decl(x, y, z, name) -> string_of_frdecl x y z name
  | Fr_print(name) -> "Print " ^ name ^ ";\n"

(* print program *)
let string_of_program stmts =  (* print program (a list of frame declarations) *)
  String.concat "" (List.rev (List.map string_of_stmt stmts))
