type action = Usage | Ast | IR | Compile

let usage =
  "\n USAGE: \n" ^
  " \t  blox.sh [-option] <input_file.blox> \n"  ^
  " OPTIONS: \n" ^
  " \t  -a   Display the Abstract Syntax Tree\n" ^
  " \t  -c   Compile input_file.blox to AMF\n"   ^
  " \t  -h   Display the Blox compiler help menu\n"

let _ =
  let action =
    if Array.length Sys.argv = 1 then
      Usage
    else if Array.length Sys.argv > 1 then
      List.assoc Sys.argv.(1)
      [ ("-h", Usage); ("-a", Ast); ("-i", IR); ("-c", Compile); ]
    else
      Compile
  in
  let file_in_chan = open_in Sys.argv.(2) in
  let lexbuf = Lexing.from_channel file_in_chan in
  let ast = Parser.program Scanner.token lexbuf in
  Analyzer.analyze ast;

  match action with
    | Usage   ->  print_endline usage
    | Ast     ->  print_endline "-----------Stack-Simulation-----------\n";     (*Testing - remove*)
                  print_endline (Ir.string_of_prog (Translator.translate ast)); (*Testing - remove*)
                  print_endline "--------------------------------------\n";     (*Testing - remove*)
                  print_string (Pprint.string_of_program ast)
    | IR      ->  print_endline (Ir.string_of_prog (Translator.translate ast))
    | Compile ->  Executor.execute (Translator.translate ast)
