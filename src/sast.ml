(* open Ast

type sexpr =
    SInt of int
  | SId of string * typ

type sstmt =
    SBlock of stmt list
  | SExpr of expr

type sfr_decl = {
  xS : int;
  yS : int;
  zS : int;
  fr_nameS : string;
}

type sface_id = {
  dimS : int * int * int;
  faceS : string;
}
type program = bind list * func_decl list
 *)