type action = Usage | Ast | LLVM_IR | Compile 

let usage =
  "\n USAGE: \n" ^
  " \t  blox.sh [-option] <input_file.blox> \n"  ^
  " OPTIONS: \n" ^
  " \t  -a   Display the Abstract Syntax Tree\n" ^
  " \t  -i   Display the LLVM IR without LLVM Analysis\n" ^
  " \t  -c   Compile input_file.blox to AMF after performing LLVM Analysis\n" ^
  " \t  -h   Display the Blox compiler help menu\n"

let _ =
  let action =
    let argc = Array.length Sys.argv in
    (* Printf.printf "Arg count: %d\n" (Array.length Sys.argv); *)
    (* if argc  1 then Usage else *)
    if argc > 1 then
      List.assoc Sys.argv.(1)
      [ ("-h", Usage); ("-a", Ast); ("-i", LLVM_IR); ("-c", Compile); ]
    else
      Compile
  in

  (* let lexbuf = Lexing.from_channel (open_in Sys.argv.(2)) in *)
  (* comment above and uncomment below to test with testsuite *)
  let lexbuf       = Lexing.from_channel stdin in
  let ast          = Parser.program Scanner.token lexbuf in
  Analyzer.analyze ast;

  match action with
    | Usage   ->  print_endline usage
    | Ast     ->  print_string (Pprint.string_of_program ast)
    | LLVM_IR ->  print_string (Llvm.string_of_llmodule (Codegen.translate ast))
    | Compile ->  let mle = Codegen.translate ast in
                  Llvm_analysis.assert_valid_module mle;
                  print_string (Llvm.string_of_llmodule mle)
    
