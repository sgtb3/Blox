module L = Llvm
module A = Ast
module I = Internal
module StringMap = Map.Make(String)

exception Error of string
exception UndefStruct of string


let context  = L.global_context ()              (* global data container *)
let blox_mod = L.create_module context "blox"   (* container *)
and i32_t    = L.i32_type   context             (* 32-bit int *)
and flt32_t  = L.float_type context             (* 32-bit float *)
and i8_t     = L.i8_type    context             (* for printf format string *)
and i1_t     = L.i1_type    context             (* 1-bit bool *)
and void_t   = L.void_type  context             (* void *)
let str_t    = L.pointer_type i8_t              (* 8-bit pointer to char *)
let size_t   = L.type_of (L.size_of i8_t)       
let null_ll  = L.const_null i32_t               (* 32-bit integer null *)
let is_main  = ref false 

(* All vds in main adopt glob name and are def as glob vars in codegen. *)
let global_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100 
(* Use our own literal lookup instead of L.lookup_global  *)
let literal_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100 
let local_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50 
(* In formal_tbl are the actual values of parameters. If need to modify
   primitives in it, should create a copy variable with the same name
   in local_tbl. *)
let formal_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10 
let array_tbl:(A.dtype, L.lltype) Hashtbl.t = Hashtbl.create 10 
let struct_tbl:(string, L.lltype) Hashtbl.t = Hashtbl.create 10 
let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create 50 
let is_struct_packed = false 

let lookup_struct sname =
  try Hashtbl.find struct_tbl sname
  with | Not_found -> raise(UndefStruct sname)

let rec ltype_of_dtype = function                   (* AST type -> LLVM type *)
  | A.Int           -> i32_t
  | A.Bool          -> i1_t
  | A.Float         -> flt32_t
  | A.String        -> str_t
  | A.Void          -> void_t
  | A.Frame(_)      -> L.pointer_type (lookup_struct "Frame")
  | A.FaceId(_)     -> L.pointer_type (lookup_struct "Face")
  | A.Array(dt,_)   -> ltype_of_dtype dt


and lookup_array (dt : A.dtype) = 
  try Hashtbl.find array_tbl dt
  with | Not_found ->
    let struct_t = 
      L.named_struct_type context ("Arr_" ^ (Pprint.string_of_dtype dt)) 
    in
    let type_array = [|size_t; L.pointer_type (ltype_of_dtype dt)|] in
    L.struct_set_body struct_t type_array is_struct_packed;
    Hashtbl.add array_tbl dt struct_t;
    struct_t 


let translate (globals,functions) =

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var map (t,n) =
      let init = L.const_int (ltype_of_dtype t) 0 in 
      StringMap.add n (L.define_global n init blox_mod) map 
    in
    let global_bindings = 
      let rec make_type_id_mapping = function
        | []       -> []
        | hd :: tl -> (match hd with 
                        | A.VarDecl(dt,id)     -> [(dt,id)]
                        | A.VarAssign(dt,id,_) -> [(dt,id)]
                        | _                    -> []) 
                       @ make_type_id_mapping (List.filter ((<>) hd) tl)
      in 
      make_type_id_mapping globals;
    in
    List.fold_left global_var StringMap.empty global_bindings 
  in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t    = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t blox_mod in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl map fdecl =
      let name = fdecl.A.fname in
      let formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_dtype t) fdecl.A.formals)
      in 
      let ftype = 
        L.function_type (ltype_of_dtype fdecl.A.typ) formal_types 
      in
      StringMap.add name (L.define_function name ftype blox_mod, fdecl) map 
    in
    List.fold_left function_decl StringMap.empty functions 
  in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =

    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    

    (* print formats for different types*)
    let int_format_str   = L.build_global_stringptr "%d\n" "fmt" builder in
    let str_format_str   = L.build_global_stringptr "%s\n" "fmt" builder in  
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in 


    (* Construct the function's "locals": formal arguments and locally
       declared variables. Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =

      let add_formal m (t, n) p = 
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_dtype t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m 
      in

      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_dtype t) n builder in 
        StringMap.add n local_var m 
      in

      let formals = 
        let param_list = Array.to_list (L.params the_function) in
        List.fold_left2 add_formal StringMap.empty fdecl.A.formals param_list
      in

      let local_bindings = 
        let rec make_type_id_mapping = function
          | []       -> []
          | hd :: tl -> (match hd with 
                          | A.VDecl(dt,id)    -> [(dt,id)]
                          | A.VAssign(va,exp) -> [(fst va,snd va)]
                          | _                 -> []) 
                         @ make_type_id_mapping (List.filter ((<>) hd) tl)
        in 
        make_type_id_mapping fdecl.A.body;
      in

      List.fold_left add_local formals local_bindings
    in


    (* Return the value for a variable or formal argument *)
    let lookup n = 
      try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    let type_of_val = function 
      | "i32*"   -> int_format_str   (*int*)
      | "i8**"   -> str_format_str   (*string*)
      | "i1*"    -> int_format_str   (*bool*)
      | "float*" -> float_format_str  (*float*)
      | _        -> str_format_str
    in 

    let check_print_input = function
      | A.Lit_Int(_)   -> int_format_str
      | A.Lit_Flt(_)   -> float_format_str
      | A.Lit_Str(_)   -> str_format_str
      | A.Lit_Bool(_)  -> int_format_str
      | A.Noexpr       -> int_format_str
      | A.Id(s)        -> type_of_val(L.string_of_lltype(L.type_of (lookup s)))
      | A.Binop(_,_,_) -> int_format_str
      | A.Unop(_,_)    -> int_format_str
      | A.Assign(_,_)  -> int_format_str
      | A.Call(_,_)    -> int_format_str
      | A.Null         -> int_format_str
    in 

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
      | A.Lit_Int(i)      -> L.const_int i32_t i
      | A.Lit_Flt(f)      -> L.const_float flt32_t f
      | A.Lit_Str(s)      -> L.build_global_stringptr s "str" builder
      | A.Lit_Bool(b)     -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr          -> L.const_int i32_t 0
      | A.Null            -> null_ll
      | A.Id(s)           -> L.build_load (lookup s) s builder
      | A.Binop(e1,op,e2) ->
	        let e1' = expr builder e1 and 
              e2' = expr builder e2 
          in
      	  (match op with
        	  | A.Add     -> L.build_add
        	  | A.Sub     -> L.build_sub
        	  | A.Mult    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.Mod     -> L.build_srem 
        	  | A.And     -> L.build_and
        	  | A.Or      -> L.build_or
        	  | A.Equal   -> L.build_icmp L.Icmp.Eq
        	  | A.Neq     -> L.build_icmp L.Icmp.Ne
        	  | A.Less    -> L.build_icmp L.Icmp.Slt
        	  | A.Leq     -> L.build_icmp L.Icmp.Sle
        	  | A.Greater -> L.build_icmp L.Icmp.Sgt
        	  | A.Geq     -> L.build_icmp L.Icmp.Sge
      	  ) e1' e2' "tmp" builder
      | A.Unop(op,e)      ->
          let e' = expr builder e in
          (match op with
      	    | A.Neg -> L.build_neg
            | A.Not -> L.build_not
          ) e' "tmp" builder
      | A.Assign(s,e) -> 
          let e' = expr builder e in
          ignore (L.build_store e' (lookup s) builder); 
          e'
      | A.Call("print",[e]) | A.Call ("printb",[e]) ->
          L.build_call 
            printf_func 
              [| int_format_str ; (expr builder e) |]
                "printf" builder
      | A.Call(f,act) ->
          let (fdef,fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = 
            (match fdecl.A.typ with 
              | A.Void -> ""
              | _      -> f ^ "_result") 
          in
          L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f = 
      match L.block_terminator (L.insertion_block builder) with
        | Some(_) -> ()
        | None    -> ignore (f builder) 
    in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
      | A.Block(sl)     -> List.fold_left stmt builder sl
      | A.Expr(e)       -> ignore (expr builder e); 
                           builder
      | A.VDecl(vd)     -> builder (*TODO *)
      | A.VAssign(vd,e) -> builder (*TODO *)
      | A.Break         -> builder (*TODO: only if in a for/while loop *)
      | A.Continue      -> builder (*TODO: only if in a for/while loop *)
      | A.Return(e)     -> 
          ignore (match fdecl.A.typ with
                    | A.Void -> L.build_ret_void builder
                    | _ -> L.build_ret (expr builder e) builder); 
          builder
      | A.If(predicate,then_stmt,else_stmt) ->
          let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          let then_bb  = L.append_block context "then" the_function in
          add_terminal 
            (stmt (L.builder_at_end context then_bb) then_stmt)
              (L.build_br merge_bb);

          let else_bb = L.append_block context "else" the_function in
          add_terminal 
            (stmt (L.builder_at_end context else_bb) else_stmt)
              (L.build_br merge_bb);
          
          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      | A.While(predicate,body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);
          
          let body_bb = L.append_block context "while_body" the_function in
          add_terminal 
            (stmt (L.builder_at_end context body_bb) body)
              (L.build_br pred_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val     = expr pred_builder predicate in
          let merge_bb     = L.append_block context "merge" the_function in

          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
      | A.For(e1,e2,e3,body) -> 
          stmt 
            builder 
              (A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3])])

    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder 
      (match fdecl.A.typ with
        | A.Void -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_dtype t) 0))
  
  in
  List.iter build_function_body functions;
  blox_mod 
