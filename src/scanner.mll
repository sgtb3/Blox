{ open Parser }

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf   }    (* Whitespace *)
  | "/* "                { comment lexbuf }    (* Comments   *)
  | '='                  { ASSIGN   }
  | ','                  { COMMA    }
  | ';'                  { SEMI     }
  | '{'                  { LCURL    }
  | '}'                  { RCURL    }
  | '('                  { LPAREN   }
  | ')'                  { RPAREN   }
  | '['                  { LBRACK   }
  | ']'                  { RBRACK   }
  | '+'                  { PLUS     }
  | '-'                  { MINUS    }
  | '*'                  { TIMES    }
  | '/'                  { DIVIDE   }
  | '%'                  { MOD      }
  | '!'                  { NOT      }
  | '.'                  { DOT      }
  | '<'                  { LT       }
  | '>'                  { GT       }
  | "!="                 { NEQ      }
  | "=="                 { EQ       }
  | "<="                 { LEQ      }
  | ">="                 { GEQ      }
  | ".="                 { FRAMEEQ  }
  | "&&"                 { AND      }
  | "||"                 { OR       }
  | "if"                 { IF       }
  | "else"               { ELSE     }
  | "for"                { FOR      }
  | "while"              { WHILE    }
  | "return"             { RETURN   }
  | "break"              { BREAK    }
  | "continue"           { CONTINUE }
  | "void"               { VOID     }
  | "null"               { NULL     }
  | "int"                { INT      }
  | "bool"               { BOOL     }
  | "true"               { TRUE     }
  | "false"              { FALSE    }
  | "string"             { STRING   }
  | "float"              { FLOAT    }
  | "print"              { PRINT    }
  | "Convert"            { CONVERT  }
  | "Build"              { BUILD    }
  | "Join"               { JOIN     }
  | "Face"               { FACE     }
  | "Frame"              { FRAME    }
  | ['0'-'9']+ as lxm    { LIT_INT(int_of_string lxm) }
  | ['0'-'9']+ '.' ['0'-'9']+ as lxm { LIT_FLT(float_of_string lxm) }
  | ['\"'] [^'\"']* ['\"'] as lxm { LIT_STR(lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_'] * as lxm { ID(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  | " */" { token lexbuf   }
  | _     { comment lexbuf }
