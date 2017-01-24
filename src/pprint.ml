open Ast

(* print binary operators *)
let str_of_op = function
  | Add     -> "+"
  | Or      -> "||"
  | Mod     -> "%"
  | And     -> "&&"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Equal   -> "=="
  | Neq     -> "!="
  | Less    -> "<"
  | Leq     -> "<="
  | Greater -> ">"
  | Geq     -> ">="

(* print unary operators *)
let str_of_uop = function
  | Neg -> "-"
  | Not -> "!"

(* print dimensions *)
let str_of_dim (x,y,z) =
  string_of_int x ^ "," ^
  string_of_int y ^ "," ^
  string_of_int z

(* print datatypes *)
let rec str_of_dtype = function
  | Int        -> "int"
  | Bool       -> "bool"
  | String     -> "string"
  | Float      -> "float"
  | Frame(fr)  -> "Frame" ^ "<" ^ str_of_dim fr.fr_dim ^ ">"
  | FaceId(fc) -> "Face"  ^ "<" ^ str_of_dim fc.fc_dim ^ "," ^ fc.face ^ ">"
  | Void       -> "void"
  | Array(x,y) -> str_of_dtype x ^ "[" ^ string_of_int y ^ "]"

(* print expressions *)
let rec str_of_expr = function
  | Lit_Int(x)      -> string_of_int x
  | Lit_Flt(x)      -> string_of_float x
  | Lit_Str(x)      -> x
  | Id(x)           -> x
  | Lit_Bool(true)  -> "true"
  | Lit_Bool(false) -> "false"
  | Null            -> "null"
  | Noexpr          -> ""
  | Assign(x,y)     -> x ^ " = " ^ str_of_expr y
  | Binop(e1,op,e2) -> str_of_expr e1 ^ " " ^ str_of_op op ^ " " ^
                       str_of_expr e2
  | Unop(op,e)      -> str_of_uop op ^ str_of_expr e
  | Call(f,el)      -> f ^ "(" ^ String.concat ", "
                       (List.map str_of_expr el) ^ ")"

(* print variable declarations *)
let str_of_var_decl (x,y) =
  str_of_dtype x ^ " " ^ y

let rec str_of_vd_list = function
  | [] -> ""
  | hd :: tl -> str_of_var_decl hd ^ ";" ^ str_of_vd_list tl

(* print variable assignments *)
let str_of_vassign (t,id,exp) =
  str_of_dtype t ^ " " ^ id ^ " = " ^ str_of_expr exp 

let rec str_of_va_list = function
  | [] -> ""
  | hd :: tl -> str_of_vassign hd ^ ";" ^ str_of_va_list tl

(* print statements *)
let rec str_of_stmt = function
  | Block(stmts)      -> " {\n\t" ^ String.concat ""
                         (List.map str_of_stmt stmts) ^ "\t}\n\n"
  | Expr(expr)        -> "\t" ^ str_of_expr expr      ^ ";\n"
  | VDecl(x,y)        -> "\t" ^ str_of_var_decl (x,y) ^ ";\n"
  | VAssign(va,ex)    -> "\t" ^ str_of_vassign (fst va,snd va,ex) ^ ";\n"
  | Return(expr)      -> "\treturn " ^ str_of_expr expr ^ ";\n";
  | If(e,s,Block([])) -> "\n\tif (" ^ str_of_expr e ^ ")" ^ str_of_stmt s
  | If(e,s1,s2)       -> "\n\tif (" ^ str_of_expr e ^ ")" ^ str_of_stmt s1 ^
                         "\telse"   ^ str_of_stmt s2
  | For(e1,e2,e3,s)   -> "\n\tfor (" ^ str_of_expr e1 ^ "; " ^ 
                          str_of_expr e2 ^ "; " ^
                          str_of_expr e3 ^ ") " ^
                          str_of_stmt s
  | While(e,s)        -> "\n\twhile (" ^ str_of_expr e  ^
                         ")"           ^ str_of_stmt s
  | Break             -> "\tbreak;\n"
  | Continue          -> "\tcontinue;\n"

(* print frame assignments *)
let str_of_frassign (fn1,fn2) =
  "Frame " ^ fn1 ^ " = " ^ fn2

let rec str_of_fra_list = function
  | [] -> ""
  | hd :: tl -> str_of_frassign hd ^ ";" ^ str_of_fra_list tl

(* print face assignments *)
let str_of_fcassign (fn1,fn2) =
  "Face " ^ fn1 ^ " = " ^ fn2 

let rec str_of_fca_list = function
  | [] -> ""
  | hd :: tl -> str_of_fcassign hd ^ ";" ^ str_of_fca_list tl

(* print function declarations *)
let str_of_func_decl fd =
  "\n" ^ str_of_dtype fd.typ ^ " " ^ fd.fname ^ "(" ^
  String.concat ", " (List.map str_of_var_decl fd.formals) ^ ") {\n" ^
  String.concat ""   (List.map str_of_stmt fd.body)        ^ "}\n"

(* print blox program *)
let str_of_program (globals,funcs) =
  String.concat "\n"
   (List.rev (List.map (fun f -> match f with
                         | VarDecl(vd)   -> str_of_vd_list  [vd]
                         | VarAssign(va) -> str_of_va_list  [va]
                         | FrAssign(fra) -> str_of_fra_list [fra]
                         | FcAssign(fca) -> str_of_fca_list [fca]
                       ) globals)) ^ "\n" ^
  String.concat "" (List.rev (List.map str_of_func_decl funcs))
