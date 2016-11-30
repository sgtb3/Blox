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

let rec string_of_expr = function         (* print expressions *)
    Int(x) -> string_of_int x
  | Id(x)  -> x
  | Assign(x, y) -> "Frame " ^ x ^ " = " ^ string_of_expr y ^ ";"

let string_of_frdecl x y z name =         (* print frame declarations *)
  "Frame<" ^ string_of_int x ^ "," ^
             string_of_int y ^ "," ^
             string_of_int z ^ "> " ^
             name ^ ";\n"

let string_of_dim (x,y,z) =
  string_of_int x ^ ", " ^
  string_of_int y ^ ", " ^
  string_of_int z

(* f  = type face_id *)
let string_of_face_id f =
  "(" ^  string_of_dim f.dim ^ f.face ^ ")"

let string_of_face_id_list fid =
  String.concat "," (List.rev (List.map string_of_face_id fid))
(*  face_id               ->  string_of_face_id face_id
  | face_id face_id_list  ->  string_of_face_id face_id ^ ", " ^
                              string_of_face_id_list face_id_list *)
(*x type = type join arg *)
let string_of_join_arg x =
  x.frname ^ "{" ^ string_of_face_id_list x.blck_face ^ "}\n"

let rec string_of_stmt = function (* print statements *)
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)   -> string_of_expr expr ^ "\n"
  | Join(x, y)   -> "Join(" ^ string_of_join_arg x ^ ", " ^ string_of_join_arg y ^ ")\n"
  | Fr_decl(x, y, z, name) -> string_of_frdecl x y z name
  | Fr_print(name) -> "Print " ^ name ^ "\n"

let string_of_program stmts =  (* print program (a list of frame declarations) *)
  String.concat "" (List.rev (List.map string_of_stmt stmts))
