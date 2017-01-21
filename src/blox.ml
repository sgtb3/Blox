type action = Usage | Ast | LLVM_IR | Compile 

let usage =
  "\n USAGE: \n" ^
  " \t  blox.sh [-option] <input_file.blox> \n"  ^
  " OPTIONS: \n" ^
  " \t  -a   Display the Abstract Syntax Tree\n" ^
  " \t  -c   Compile input_file.blox to AMF\n"   ^
  " \t  -h   Display the Blox compiler help menu\n"

let _ =
  let action =
    Printf.printf "Args passed: %d\n" (Array.length Sys.argv);
    if Array.length Sys.argv = 1 then
      Usage
    else if Array.length Sys.argv > 1 then
      List.assoc Sys.argv.(1)
      [ ("-h", Usage); ("-a", Ast); ("-i", LLVM_IR); ("-c", Compile); ]
    else
      Compile
  in
  let file_in_chan = open_in Sys.argv.(2) in
  let lexbuf       = Lexing.from_channel file_in_chan in
  let ast          = Parser.program Scanner.token lexbuf in
  Analyzer.analyze ast;

  match action with
    | Usage   ->  print_endline usage
    | Ast     ->  print_string (Pprint.string_of_program ast)
    | LLVM_IR ->  print_string (Llvm.string_of_llmodule (Codegen.translate ast))
    | Compile ->  let mle = Codegen.translate ast in
                  Llvm_analysis.assert_valid_module mle;
                  print_string (Llvm.string_of_llmodule mle)
    
