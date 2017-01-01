open Ast

(* print binary operators *)
let string_of_op = function
  | Add     -> "+"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Equal   -> "=="
  | Neq     -> "!="
  | Less    -> "<"
  | Leq     -> "<="
  | Greater -> ">"
  | Geq     -> ">="
  | And     -> "&&"
  | Or      -> "||"
  | FrameEq -> ".="
  | Mod     -> "%"

(* print unary operators *)
let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"

(* print dimensions *)
let string_of_dim (x,y,z) = 
  string_of_int x ^ "," ^ 
  string_of_int y ^ "," ^ 
  string_of_int z

(* print datatypes *)
let rec string_of_dtype = function
  | Int          -> "int"
  | Bool         -> "bool"
  | String       -> "string"
  | Float        -> "float"
  | Frame(fr)    -> "Frame" ^ "<" ^ string_of_dim (fr.x, fr.y, fr.z) ^ ">"
  | FaceId(fc)   -> "Face"  ^ "<" ^ string_of_dim fc.dim ^ "," ^ fc.face ^ ">"
  | Void         -> "void"
  | Array(x,y,z) -> string_of_dtype x ^ "[" ^ string_of_int y ^ "] " ^ z

(* print expressions *)
let rec string_of_expr = function         
  | Lit_Int(x)        -> string_of_int   x
  | Lit_Flt(x)        -> string_of_float x
  | Lit_Str(x)        -> x
  | Id(x)             -> x
  | Lit_Bool(true)    -> "true"
  | Lit_Bool(false)   -> "false"
  | Null              -> "null"
  | Noexpr            -> ""
  | Assign(x,y)       -> x ^ " = " ^ string_of_expr y ^ ";"
  | Fr_assign(x,y)    -> "Frame "  ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Fc_assign(x,y)    -> "Face "   ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Var_assign(x,y,z) -> string_of_dtype x ^ " " ^ y ^ " = " ^ 
                         string_of_expr z  ^ ";"
  | Binop(e1,o,e2)    -> string_of_expr e1 ^ " " ^ 
                         string_of_op   o  ^ " " ^ 
                         string_of_expr e2
  | Unop(o,e)         -> string_of_uop o   ^ string_of_expr e
  | Call(f,el)        -> f ^ "(" ^ String.concat ", " 
                         (List.map string_of_expr el) ^ ");"

(* print variable declarations *)
let string_of_var_decl (x,y) =
  string_of_dtype x ^ " " ^ y

(* print block face identifiers *)
let string_of_face_id (w,x,y,z) =
  "(" ^ string_of_int w ^ ", " ^ 
        string_of_int x ^ ", " ^ 
        string_of_int y ^ ", " ^ z ^ ")"

(* print Join function arguments *)
let string_of_join_args (w,x,y,z) =
  w.fr_id ^ "," ^ x.fc_id ^ "," ^ y.fr_id ^ "," ^ z.fc_id 

(* print Build function arguments *)
let string_of_build_args (w,x,y,z) =
  w.fr_id ^ "," ^ (String.concat "," (List.map (fun f -> f.fc_id) x)) ^ "," ^
  y.fr_id ^ "," ^ (String.concat "," (List.map (fun f -> f.fc_id) z))

let tab str = "\t" ^ str

(* print statements *)
let rec string_of_stmt = function 
  | Var_decl(x,y)     -> "\t"^ string_of_var_decl (x,y) ^ ";\n"
  | Expr(expr)        -> "\t"^ string_of_expr expr      ^ "\n"
  | Join(w,x,y,z)     -> "\t"^ "Join("  ^ string_of_join_args  (w,x,y,z) ^ ");\n"
  | Build(w,x,y,z)    -> "\t"^ "Build(" ^ string_of_build_args (w,x,y,z) ^ ");\n"
  | Array(x,y,z)      -> "\t"^ string_of_dtype x ^ "[" ^ 
                         string_of_int y   ^ "] " ^ z ^";\n"
  | Print(e)          -> "\t"^ "print(" ^ string_of_expr e ^ ");\n"
  | Convert(fr)       -> "\t"^ "Convert(" ^ fr.fr_id ^ ");\n"
  | Break             -> "\t"^ "break;\n"
  | Continue          -> "\t"^ "continue;\n"
  | Return(expr)      -> "\treturn " ^ string_of_expr expr ^ ";\n";
  | If(e,s,Block([])) -> "\n\tif ("    ^ string_of_expr e  ^ 
                         ")"     ^ string_of_stmt s
  | If(e,s1,s2)       -> "\n\tif ("    ^ string_of_expr e  ^ 
                         ")"     ^ string_of_stmt s1 ^ 
                         "\telse\n"  ^ string_of_stmt s2
  | For(e1,e2,e3,s)   -> "\n\tfor (" ^ string_of_expr e1 ^ "; "  
  									 ^ string_of_expr e2 ^ "; "  
  									 ^ string_of_expr e3 ^ ") "  
  									 ^ string_of_stmt s
  | While(e,s)        -> "\n\twhile (" ^ string_of_expr e  ^ 
                         ")"          ^ string_of_stmt s
  | Block(stmts)      -> " {\n\t" ^ String.concat "" 
                                (List.map string_of_stmt stmts) ^ "\t}\n\n"

(* print variable assignments *) 
let string_of_vassign (t,id,exp) = 
  string_of_dtype t ^ " " ^ id ^ " = " ^ string_of_expr exp ^ ";"

(* print frame assignments *)
let string_of_frassign (fn1,fn2) = 
  "Frame " ^ fn1 ^ " = " ^ fn2 ^ ";"

(* print face assignments *)
let string_of_fcassign (fn1,fn2) = 
  "Face " ^ fn1 ^ " = " ^ fn2 ^ ";"
(* 
let string_of_formals fd = 

  let l1 = List.map fst fd.formals in
  let l2 = List.map snd fd.formals in
  List.iter2 String.concat " " l1 l2
   *)

(* print function declarations *)
let string_of_func_decl fd =
  "\n" ^ string_of_dtype fd.typ ^ " " ^ fd.fname ^ "(" ^ 
  String.concat ", " (List.map snd fd.formals)         ^ ") {\n" ^
  String.concat "" (List.map string_of_stmt fd.body) ^ "}\n"

let rec string_of_vd = fun list -> match list with
  | [(dt,id)]     -> string_of_var_decl (dt,id) ^ ";"
  | []            -> "" 
  | hd :: tl      -> string_of_var_decl hd ^ "" ^ string_of_vd tl

let rec string_of_va = fun list -> match list with
  | [(dt,id,exp)] -> string_of_vassign (dt,id,exp) 
  | []            -> "" 
  | hd :: tl      -> string_of_vassign hd ^ "" ^ string_of_va tl

let rec string_of_fra = fun list -> match list with
  | [(id1,id2)]   -> string_of_frassign (id1,id2) 
  | []            -> "" 
  | hd :: tl      -> string_of_frassign hd ^ "\n" ^ string_of_fra tl

let rec string_of_fca = fun list -> match list with
  | [(id1,id2)]   -> string_of_fcassign (id1,id2) 
  | []            -> "" 
  | hd :: tl      -> string_of_fcassign hd ^ "\n" ^ string_of_fca tl

(* print blox program *)
let string_of_program (globals,funcs) = 
  String.concat "\n" 
   (List.rev (List.map (fun f -> match f with
	                     | VarDecl(vd)   -> string_of_vd  [vd]
	                     | VarAssign(va) -> string_of_va  [va]
	                     | FrAssign(fra) -> string_of_fra [fra]
	                     | FcAssign(fca) -> string_of_fca [fca]
	                     | NoGlob        -> "") globals)) ^ "\n" ^
  String.concat "" (List.rev (List.map string_of_func_decl funcs))
