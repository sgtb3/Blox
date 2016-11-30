module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(* let translate (globals, functions) = *)
let translate(frame_dcls) = 
  let context = L.global_context () 
  in
  let the_module = 
    L.create_module context "MicroC"
      and i32_t  = L.i32_type  context
      and i8_t   = L.i8_type   context
      and i1_t   = L.i1_type   context
      and void_t = L.void_type context 
  in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t 
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in 
      StringMap.add n (L.define_global n init the_module) m 
    in
    List.fold_left global_var StringMap.empty globals 
  in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] 
  in
  let printf_func = L.declare_function "printf" printf_t the_module 
  in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name =  fdecl.A.fname and 
                  formal_types =
                  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in 
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types 
      in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m 
    in
    List.fold_left function_decl StringMap.empty functions 
  in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls 
    in
    let builder = L.builder_at_end context (L.entry_block the_function) 
    in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
    in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;

  let local = L.build_alloca (ltype_of_typ t) n builder 
  in
  ignore (L.build_store p local builder);
  StringMap.add n local m 
  in

  let add_local m (t, n) =
    let local_var = L.build_alloca (ltype_of_typ t) n builder
    in StringMap.add n local_var m 
  in

  let formals = 
    List.fold_left2 add_formal StringMap.empty fdecl.A.formals (Array.to_list (L.params the_function)) 
  in
  List.fold_left add_local formals fdecl.A.locals 
in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	A.Literal i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  List.iter build_function_body functions;
  the_module

(* AMF Code Generator for the Blox compiler. Takes a semantically checked AST 
(SAST) and produces an AMF file. *)

(* Work in progress faceCheck funcion*)
(*

let arr = Array.make 3 (Array.make 3 (Array.make 3 0));;

let faceCheck a = 
  
  let x = ref 0 
  in

  for i = 0 to Array.length a - 1 do
    let y = ref 0 and xarr = a.(i) 
    in
    for j = 0 to Array.length xarr - 1 do
      let z = ref 0 and yarr = xarr.(j) 
      in
      for k = 0 to Array.length a.(i).(j) - 1 do
        yarr.(k) <- !x + !y + !z;
        incr z
      done;
      incr y
    done;
    incr x
  
  done;

;;
*)

(*JOIN FXN*)
(*UNCHECKED AND UNTESTED*)
open Dynarray
let join frameA (a,b,c,d) frameB (e,f,g,h) = 
	let ax = a in
	let ay = b in
	let az = c in
	let afacetr = d in
	
	let bx = e in
	let by = f in
	let bz = g in
	let bfacetr = h in
	
	let ax_shift = 0 in
	let ay_shift = 0 in
	let az_shift = 0 in
	
	let bx_shift = 0 in
	let by_shift = 0 in
	let bz_shift = 0 in 

	if frameA = frameB then prerr_string "Error: Attempting to join blocks from the same Frame.";  
			
	let aface = false in 
	
	if afacetr = "E" then aface = frameA[ax][ay][az].open_faces[0]
		else
	if afacetr = "W" then aface = frameA[ax][ay][az].open_faces[1]
		else
	if afacetr = "N" then aface = frameA[ax][ay][az].open_faces[2]
		else
	if afacetr = "S" then aface = frameA[ax][ay][az].open_faces[3]
		else
	if afacetr = "F" then aface = frameA[ax][ay][az].open_faces[4]
		else
	if afacetr = "B" then aface = frameA[ax][ay][az].open_faces[5] 

	let bface = false in
	
	if bfacetr = "E" then(
		bface = frameB[bx][by][bz].open_faces[0];
		bx_shift = (ax - 1) - bx;
		by_shift = ay - by;
		bz_shift = az - bz)
		
	else if bfacetr = "W" then( 
		bface = frameB[bx][by][bz].open_faces[1];
		bx_shift = (ax + 1) - bx;
		by_shift = ay - by;
		bz_shift = az - bz)
		
	else if bfacetr = "N" then( 
		bface = frameB[bx][by][bz].open_faces[2];
		bx_shift = ax - bx;
		by_shift = (ay - 1) - by;
		bz_shift = az - bz)
		
	else if bfacetr = "S" then( 
		bface = frameB[bx][by][bz].open_faces[3];
		bx_shift = ax - bx;
		by_shift = (ay + 1) - by;
		bz_shift = az - bz) 
		
	else if bfacetr = "F" then( 
		bface = frameB[bx][by][bz].open_faces[4];
		bx_shift = ax - bx;
		by_shift = ay - by;
		bz_shift = (az - 1) - bz)
		
	else if bfacetr = "B" then( 
		bface = frameB[bx][by][bz].open_faces[5];
		bx_shift = ax - bx;
		by_shift = ay - by;
		bz_shift = (az + 1) - bz) in
	
	(* check if frameA's block face is available *)
	if not(aface) then prerr_string; "Error: Block face is not available for Join with";
	
	(* check if frameB's block face is available *)
	if not(bface) then prerr_string; "Error: Block face is not available for Join with";
	
	(* check for opposite faces *)
	if (((afacetr = "E") && not(bfacetr = "W")) ||
			((afacetr = "W") && not(bfacetr "W")))
		then prerr_string "Error: Illegal face option.";

	if (((afacetr = "N") && not(bfacetr = "S")) ||
		  ((afacetr = "S") && not(bfacetr "N")))
		then prerr_string "Error: Illegal face option.";
	
	if (((afacetr = "N") && not(bfacetr = "S")) ||
		  ((afacetr = "S") && not(bfacetr "N")))
		then prerr_string "Error: Illegal face option.";

	(*  ========== ALL CHECKS PASSED. BEGIN JOIN PROCESS ========== *)
		
  (* create an entry for Frame "joins" list *)		
	
	type joins_entry = {frameone:frame; coordinatesone:int*int*int*int; frametwo:frame; coordinatestwo:int*int*int*int} in
	frameA.joins.add = join_entry in  (* add the join entry into A's joins *)
  frameB.joins.add = join_entry in  (* add the join entry into B's joins *)
	
	(* Determine shift values for A and B *)
	if bx_shift < 0 then(
		ax_shift = -bx_shift;
		bx_shift = 0;)
	
	if by_shift < 0 then(
		ay_shift = -by_shift;
		by_shift = 0;)
		
	if bz_shift < 0 then(
		az_shift = -bz_shift;
		bz_shift = 0;)
		
	(* Determine size of new array *)

	let cx_max = frameA.x + ax_shift in
	if cx_max < (frameB.x + bx_shift) then 
		cx_max = frameB.x + bx_shift;		
		
	let cy_max = frameA.y + ay_shift in
	if cy_max < (frameB.y + by_shift) then 
		cy_max = frameB.y + by_shift;

	let cz_max = frameA.z + az_shift in
	if cz_max < (frameB.z + bz_shift) then 
		cz_max = frameB.z + bz_shift;		
		
  (* create an entry for Frame "joins" list *)		
	
	
	(* OLD STUFF BELOW *)
(*
	/****************
* helloworld.blox
****************/
type blck = {faces : bool array};;
let blck1 = {faces = [|false;false;false;false;false;false|]};;
let frame
let frame1 = Array.init 3 (fun _ -> Array.init 3 (fun _ -> (Array.make 3 blck1)));;

A.get(Ax).get(Ay).get(Az).open_faces[0];"
A[Ax][Ay][Az].open_faces[0];

frame1.blck1.faces[1][1][1]
hat = frame1[ax][ay][az]

type faces = {one:bool; two:bool};;
let blockfaces = {one=true; two=false};;
/* Create basic frames to construct letters with */
Frame<1,1,1> one;
Frame<2,1,1> twox;
Frame<1,2,1> twoy;
Frame<3,1,1> threex;
Frame<1,3,1> threey;
Frame<1,5,1> fivey;
Frame<46,5,1> base;

    // This is a Compiler function
    private static void Join(Frame A, int[] A_coord, String A_face,
                             Frame B, int[] B_coord, String B_face) {
// FUNCTION CALL
# let join frameA (a,b,c,d) frameB (e,f,g,h) = abs (a - b);;
val join : 'a -> int * int * 'b * 'c -> 'd -> 'e * 'f * 'g * 'h -> int = <fun>

type faces = {east:bool; west:bool; north:bool; south:bool; front:bool; back:bool};;
type frame = {x:int; y:int; z:int; name:string; sides:faces};;
let frameA = {x=1; y=1; z=1; name="one"; sides ={east = true; west = true; north = true; south = true; front = true; back = true}};;
let frameB = {x=2; y=1; z=1; name="twox"; sides ={east = true; west = true; north = true; south = true; front = true; back = true}};;
let frameC = {x=1; y=1; z=1; name="one"};;
 
let join frameA (a,b,c,d) frameB (e,f,g,h) = 
	let ax = a in
	let ay = b in
	let az = c in
	let achar = d in
	
	let bx = e in
	let by = f in
	let bz = g in
	let bchar = h in
	
	let axshift = 0 in
	let ayshift = 0 in
	let azshift = 0 in
	
	let bxshift = 0 in
	let byshift = 0 in
	let bzshift = 0 in 
	

	if frameA = frameB then print_string "Error: Attempting to join blocks from the same Frame." 
		else 
			
	let aface = false in 

  if frameA.name = "E" then aface = frameA.sides.east
		else
	if frameA.name = "W" then aface = frameA.sides.west
		else
	if frameA.name = "N" then aface = frameA.sides.north
		else
	if frameA.name = "S" then aface = frameA.sides.south
		else
	if frameA.name = "F" then aface = frameA.sides.front
		else
	if frameA.name = "B" then aface = frameA.sides.back 
    and aface = false;; in 
		
	let check2() = 
  let bface = false;; in
	
  if frameB.name = "E" then bface = frameB.sides.east in
		bxshift = (ax - 1) - bx in
		byshift = ay - by in
		bzshift = az - bz 
		else
	if frameB.name = "W" then bface = frameB.sides.west in
		bxshift = (ax + 1) - bx in
		byshift = ay - by in
		bzshift = az - bz 
		else
	if frameB.name = "N" then bface = frameB.sides.north in
		bxshift = ax - bx in
		byshift = (ay - 1) - by in
		bzshift = az - bz 
		else
	if frameB.name = "S" then bface = frameB.sides.south in
		bxshift = ax - bx in
		byshift = (ay + 1) - by in
		bzshift = az - bz 
		else
	if frameB.name = "F" then bface = frameB.sides.front in
		bxshift = ax - bx in
		byshift = ay - by in
		bzshift = (az - 1) - bz 
		else 
	if frameB.name = "B" then bface = frameB.sides.back in
		bxshift = ax - bx in
		byshift = ay - by in
		bzshift = az - bz 
		;;
		
		

  
	  let bface = false;; 
	
  if frameB.name = "E" then (bface = frameB.sides.east) and
		let bxshift = (2 - 1) in
		let byshift = 3- 4 in
		let bzshift = 6 - 22;; 
	
	
	
	

														 

/* Create an H */
Frame H = fivey;
Join(H, (1,3,1,E), twox, (1,1,1,W));
Join(H, (3,3,1,E), fivey, (1,3,1,W));

/* Create an E */
Frame E = fivey;
Join(E, (1,1,1,E), twox, (1,1,1,W));
Join(E, (1,3,1,E), twox, (1,1,1,W));
Join(E, (1,5,1,E), twox, (1,1,1,W));

/* Create an L */
Frame L = fivey;
Join(L, (1,1,1,E), twox, (1,1,1,W));

/* Create an O */
Frame O = fivey;
Join(O, (1,1,1,E), twox, (1,1,1,W));
Join(O, (1,5,1,E), twox, (1,1,1,W));
Join(O, (3,1,1,E), fivey, (1,1,1,W));

/* Create a W */
Frame W = fivey;
Join(W, (1,1,1,E), threex, (1,1,1,W));
Join(W, (4,1,1,E), fivey, (1,1,1,W));
Join(W, (3,1,1,N), twoy, (1,1,1,S));

/* Create an R */
Frame R = threey;
Join(R,(1,3,1,E), twox, (1,1,1,W));

/* Create an D */
Frame D = threey;
Join(D,(1,1,1,E), one, (1,1,1,W));
Join(D,(1,3,1,E), one, (1,1,1,W));
Join(D,(2,1,1,E), fivey, (1,1,1,W));

/* Join letters to the base */
Join(base,(1,1,1,F), H, (1,1,1,B));
Join(base,(6,1,1,F), E, (1,1,1,B));
Join(base,(10,1,1,F), L, (1,1,1,B));
Join(base,(14,1,1,F), L, (1,1,1,B));
Join(base,(18,1,1,F), O, (1,1,1,B));
Join(base,(25,1,1,F), W, (1,1,1,B));
Join(base,(31,1,1,F), O, (1,1,1,B));
Join(base,(36,1,1,F), R, (1,1,1,B));
Join(base,(40,1,1,F), L, (1,1,1,B));
Join(base,(44,1,1,F), D, (1,1,1,B));

print base;

/* Need to specify which frame is supposed to be converted to amf??*/
let join frameA (a,b,c,d) frameB (e,f,g,h) = 
	let ax = a in
	let ay = b in
	let az = c in
	let achar = d in
	
	let bx = e in
	let by = f in
	let bz = g in
	let bchar = h in
	
	let axshift = 0 in
	let ayshift = 0 in
	let azshift = 0 in
	
	let bxshift = ref 0 in
	let byshift = ref 0 in
	let bzshift = ref 0 in 

	if frameA = frameB then print_string "Error: Attempting to join blocks from the same Frame." 
		else

let bface = false in
  if frameB.name = "E" then 
	(bface = frameB.sides.east; 
		bxshift := (ax - 1) - bx;
		byshift := ay - by;
		bzshift := az - bz)
		
		else if frameB.name = "W" then 
		(bface = frameB.sides.west;
		bxshift := (ax + 1) - bx;
		byshift := ay - by;
		bzshift := az - bz)
		
		else if frameB.name = "N" then 
		(bface := frameB.sides.north;
		bxshift := ax - bx;
		byshift := (ay - 1) - by;
		bzshift := az - bz)
		
		else if frameB.name = "S" then 
		(bface := frameB.sides.south;
		bxshift := ax - bx;
		byshift := (ay + 1) - by;
		bzshift := az - bz) 
		
		else if frameB.name = "F" then 
		(bface = frameB.sides.front;
		bxshift := ax - bx;
		byshift := ay - by;
		bzshift := (az - 1) - bz)
		
		else if frameB.name = "B" then 
		(bface := frameB.sides.back;
		bxshift := ax - bx;
		byshift := ay - by;
		bzshift := az - bz);;
*)
		
	
	
	
	
	
	
	