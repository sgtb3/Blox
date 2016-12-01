open Ast

type sexpr =
    SIntLit of int
  | SId of string * primi_typ

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

type program =
  fr_decl list
