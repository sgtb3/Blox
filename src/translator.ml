(* Rules for translating code into executable *)
open Ast
open Ir

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
  function_index : int StringMap.t; (* Index for each function *)
  global_index   : int StringMap.t; (* "Address" for global variables *)
  local_index    : int StringMap.t; (* FP offset for args, locals *)
}

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
  | []       -> []
  | hd :: tl -> (n, hd) :: enum stride (n + stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate (globals, functions) =

  (* testing only - transform globs to string list of integer va/vd ids only *)
  let globals = 
    let rec make_type_id_mapping = function
      | []       -> []
      | hd :: tl -> (match hd with 
                      | VarDecl(dt,id)     -> if dt = Int then [(dt,id)] else []
                      | VarAssign(dt,id,e) -> if dt = Int then [(dt,id)] else []
                      | _                  -> []) 
                     @ make_type_id_mapping (List.filter ((<>) hd) tl)
    in 
    List.map snd (make_type_id_mapping globals)
  in
  
  (* Allocate "addresses" for each global variable *)
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in

  (* Assign indexes to function names; built-in "print" is special *)
  let built_in_functions = StringMap.add "max" (-1) StringMap.empty in
  let built_in_functions = StringMap.add "print" (-1) built_in_functions in
  let function_indexes   = 
    string_map_pairs 
      built_in_functions 
        (enum 1 1 (List.map (fun f -> f.fname) functions)) 
  in

  (* Translate a function in AST form into a list of IR statements *)
  let compile env fdecl =

    (* TESTING DELETE ME *)
    let local_bindings = 
      let rec make_int_id_list = function
        | []       -> []
        | hd :: tl -> (match hd with 
          | Var_Decl(dt,id)    -> if dt = Int then [(dt,id)] else []
          | Var_Assign(va,exp) -> if fst va = Int then [(fst va,snd va)] else []
          | _                  -> []) 
        @ make_int_id_list (List.filter ((<>) hd) tl)
      in 
      List.map snd (make_int_id_list fdecl.body)
    in
    (* let _ = List.map print_endline local_bindings in *)

    (* Make a list of only the int formal vars (for now) *)
    let formal_bindings = 
      let rec make_int_id_list = function
        | []       -> []
        | hd :: tl -> (match hd with (dt,id) -> 
                       if dt = Int then [(dt,id)] else []) 
        @ make_int_id_list (List.filter ((<>) hd) tl)
      in 
      List.map snd (make_int_id_list fdecl.formals)
    in
    (* let _ = List.map print_endline formals_bindings in *)


    (* Bookkeeping: frame pointer offsets for locals and arguments *)
    let 
      num_formals    = List.length formal_bindings and 
      num_locals     = List.length local_bindings and 
      local_offsets  = enum 1 1 local_bindings    and 
      formal_offsets = enum (-1) (-2) formal_bindings 
    in

    let 
      env = { 
        env with local_index = string_map_pairs
        StringMap.empty (local_offsets @ formal_offsets) 
      } 
    in

    let rec expr = function
      | Lit_Int(i)        -> [Lit i]
      | Id(s)             -> 
         (try [Lfp (StringMap.find s env.local_index)]
          with Not_found  -> try [Lod (StringMap.find s env.global_index)]
          with Not_found  -> raise (Failure ("undeclared variable " ^ s)))
      | Binop(e1,op,e2)   -> expr e1 @ expr e2 @ [Bin op]
      | Assign(s,e)       -> 
        expr e @
          (try [Sfp (StringMap.find s env.local_index)]
           with Not_found -> try [Str (StringMap.find s env.global_index)]
           with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Call(fn,actuals)  -> (try
          (List.concat (List.map expr (List.rev actuals))) @
          [Jsr (StringMap.find fn env.function_index) ]   
        with Not_found    -> raise (Failure ("undefined function " ^ fn)))
      | Noexpr            -> []

    in 
    let rec stmt = function
      | Block(st)       -> List.concat (List.map stmt st)
      | Expr(ex)        -> expr ex @ [Drp]
      | Return(ex)      -> expr ex @ [Rts num_formals]
      | If(p,t,f)       -> 
          let t' = stmt t and f' = stmt f in
          expr p @ [Beq(2 + List.length t')] @
          t' @ [Bra(1 + List.length f')] @ f'
      | For(e1,e2,e3,b) -> stmt (Block([Expr(e1); While(e2, Block([b; Expr(e3)]))]))
      | While(e,b)      ->
          let b' = stmt b and e' = expr e in
          [Bra (1+ List.length b')] @ b' @ e' @
          [Bne (-(List.length b' + List.length e'))]
          in [Ent num_locals] @      (* Entry: allocate space for locals *)
          stmt (Block fdecl.body) @  (* Body *)
          [Lit 0; Rts num_formals]   (* Default = return 0 *)
    in 

    let 
      env = { 
        function_index = function_indexes;
        global_index   = global_indexes;
        local_index    = StringMap.empty 
      } 
    in

    (* Code executed to start the program: Jsr main; halt *)
    let entry_function = [Jsr (StringMap.find "main" function_indexes); Hlt] in
    
    (* Compile the functions *)
    let func_bodies = entry_function :: List.map (compile env) functions in

    (* Calculate function entry points by adding their lengths *)
    let (fun_offset_list, _) = 
      List.fold_left
        (fun (l, i) f -> (i :: l, (i + List.length f))) ([], 0) func_bodies 
    in

    let func_offset = Array.of_list (List.rev fun_offset_list) in

    { 
      num_globals = List.length globals;
      (* Concatenate the compiled functions and replace the function
         indexes in Jsr statements with PC values *)
      text =  Array.of_list 
                (List.map (function
                            | Jsr i when i > 0 -> Jsr func_offset.(i)
                            | _ as s -> s) (List.concat func_bodies)) 
    }
