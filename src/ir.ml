open Ast

type bstmt =
  | Lit of int                (* Push a literal *)
  | Drp                       (* Discard a value *)
  | Bin of op                 (* Perform arithmetic on top of stack *)
  | Lod of int                (* Fetch global variable *)
  | Str of int                (* Store global variable *)
  | Lfp of int                (* Load frame pointer relative *)
  | Sfp of int                (* Store frame pointer relative *)
  | Jsr of int                (* Call function by absolute address *)
  | Ent of int                (* Push FP, FP -> SP, SP += i *)
  | Rts of int                (* Restore FP, SP, consume formals, push result *)
  | Beq of int                (* Branch relative if top-of-stack is zero *)
  | Bne of int                (* Branch relative if top-of-stack is non-zero *)
  | Bra of int                (* Branch relative *)
  | Hlt                       (* Terminate *)

type prog = {
  num_globals : int;          (* Number of global variables *)
  text        : bstmt array;  (* Code for all the functions *)
}

let string_of_stmt = function
  | Lit(i)       -> "Lit " ^ string_of_int i
  | Drp          -> "Drp"
  | Bin(Add)     -> "Add"
  | Bin(Sub)     -> "Sub"
  | Bin(Mult)    -> "Mul"
  | Bin(Div)     -> "Div"
  | Bin(Equal)   -> "Eql"
  | Bin(Neq)     -> "Neq"
  | Bin(Less)    -> "Lt"
  | Bin(Mod)     -> "Mod"
  | Bin(Leq)     -> "Leq"
  | Bin(Greater) -> "Gt"
  | Bin(Geq)     -> "Geq"
  | Lod(i)       -> "Lod " ^ string_of_int i
  | Str(i)       -> "Str " ^ string_of_int i
  | Lfp(i)       -> "Lfp " ^ string_of_int i
  | Sfp(i)       -> "Sfp " ^ string_of_int i
  | Jsr(i)       -> "Jsr " ^ string_of_int i
  | Ent(i)       -> "Ent " ^ string_of_int i
  | Rts(i)       -> "Rts " ^ string_of_int i
  | Bne(i)       -> "Bne " ^ string_of_int i
  | Beq(i)       -> "Beq " ^ string_of_int i
  | Bra(i)       -> "Bra " ^ string_of_int i
  | Hlt          -> "Hlt"

let string_of_prog prog =
  string_of_int prog.num_globals ^ " global variables\n\n" ^
  let funca =
    Array.mapi (fun i s -> string_of_int i ^ " " ^ string_of_stmt s) prog.text
  in
  String.concat "\n" (Array.to_list funca)
