type action = Usage | Ast | Execute | Compile 

let usage = 
  "\n USAGE: \n" ^
  " \t  blox.sh [-option] <input_file.blox> \n"  ^
  " OPTIONS: \n" ^
  " \t  -a   Display the Abstract Syntax Tree\n" ^
  " \t  -c   Compile input_file.blox to AMF\n"   ^
  " \t  -h   Display the Blox compiler help menu\n"

let _ =
  let action = 
    if Array.length Sys.argv > 1 then
      List.assoc Sys.argv.(1) 
      [ ("-h", Usage); ("-a", Ast); ("-e", Execute); ("-c", Compile); ] 
    else 
      Compile
  in
  let file_in_chan = open_in Sys.argv.(2)
  in
  let lexbuf = Lexing.from_channel file_in_chan
  in
  let ast = Parser.program Scanner.token lexbuf 
  in
  Analyzer.analyze ast;

  match action with
    | Usage   ->  print_endline usage
    | Ast     ->  print_string (Ast.string_of_program ast)
    | Execute ->  Executor.execute ast
    | Compile ->  Generator.generate ast
    (* | Compile ->  let e = Executor.execute ast 
                  in
                  Generator.generate e *)
