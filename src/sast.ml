open Ast

type expr =  
    IntLit of int 
  | Id of string * primi_typ

type stmt = 
    Block of stmt list 
  | Expr of expr

type fr_decl = { 
  x : int;
  y : int;
  z : int;
  fr_name : string;
}

type program = fr_decl list
