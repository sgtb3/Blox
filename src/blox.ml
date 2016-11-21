(* Top-level of the Blox compiler: scan & parse the input, check the resulting 
  AST, and generate the AMF file as output *)

(* Need to get *)
type action = Usage | Ast | Compile | AMF

let usage = 
  "\n USAGE: \n" ^
  " \t  blox.sh [-option] <input_file.blox> \n" ^
  " OPTIONS: \n" ^
  " \t  -a   Display the Abstract Syntax Tree\n" ^
  " \t  -c   Compile input_file.blox to output_file.amf\n"

let _ =
  let action, blox_file = 
    if Array.length Sys.argv > 1 then
      List.assoc Sys.argv.(1) 
      [ ("-a", Ast); ("-c", Compile) ] (* -a: print AST only; -c gen AMF file *)
    else 
      Compile 
  in
  let file_in_chan = open_in blox_file
  in
  let lexbuf = Lexing.from_channel file_in_chan 
  in
  let ast = Parser.program Scanner.token lexbuf 
  in
  analyzer.check ast;
  
  match action with
    | Usage   ->  print_endline usage
    | Ast     ->  print_string (Ast.string_of_program ast)
    | Compile ->  let m = Generator.translate ast 
                  in
                  print_string (Generator.string_of_module m)
