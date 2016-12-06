type action = Usage | Ast | Compile | Execute

let usage = 
  "\n USAGE: \n" ^
  " \t  blox.sh [-option] <input_file.blox> \n" ^
  " OPTIONS: \n" ^
  " \t  -a   Display the Abstract Syntax Tree\n" ^
  " \t  -c   Compile input_file.blox to output_file.amf\n" ^
  " \t  -h   Display the Blox compiler help menu\n"

let _ =
  let action = 
    if Array.length Sys.argv > 1 then
      List.assoc Sys.argv.(1) [ ("-a", Ast); ("-c", Compile); ("-h", Usage); ("-e", Execute)] 
    else 
      Compile
  in
  let file_in_chan = open_in Sys.argv.(2)
  in
  let lexbuf = Lexing.from_channel file_in_chan
  in
  let ast = Parser.program Scanner.token lexbuf 
  in
  Analyzer.run ast;
  
  match action with
    | Usage   ->  print_endline usage
    | Ast     ->  print_string (Ast.string_of_program ast)
    | Compile ->  print_endline "compile not implemented yet"
    | Execute ->  Executor.execute ast
    (* | Execute ->  Generator.translate Executor.execute ast *)
    (* | Compile ->  let m = Generator.translate ast 
                  in
                  print_string (Generator.string_of_module m) *)

(* Top-level of the Blox compiler: scan & parse the input, check the resulting 
  AST, and generate the AMF file as output *)