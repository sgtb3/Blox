open Ast
open Sast
open Printf

exception Face_Taken of string;;
exception Block_Overlap of string;;
exception Invalid_Face of string;;
exception Opposite_Face of string;;
exception Invalid_Block of string;;
exception Block_Out_Of_Bounds of string;;
(*
let x = ref 0 in
let y = ref 0 in 
let z = ref 0 in
let vertices = ref [|0; 1; 2; 3; 4; 5; 6; 7|] in
let line = ref 8 in
let generate = function 
	| Fr_print f1 -> 
		let _ =						 	  				
		let oc = open_out "test.amf" in   	(* create or truncate file, return channel *)
			let top = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<amf>\n\t<object id=\"1\">\n\t\t<mesh>" in 
			let bottom = "\t\t\t</volume>\n\t\t</mesh>\n\t</object>\n</amf>" in   
			let vertexstart = "\t\t\t<vertices>" in
			let vertexend = "\t\t\t</vertices>" in
			let vpart1 = "<vertex><coordinates><x>" in
			let vpart2 = "</x><y>" in 
			let vpart3 = "</y><z>" in
			let vpart4 = "</z></coordinates></vertex>" in
			let trianglestart = "\t\t\t<volume>" in
			let triangleend = "\t\t\t</volume>" in
			let tpart1 = "<triangle><v1>" in
			let tpart2 = "</v1><v2>" in
			let tpart3 = "</v2><v3>" in
			let tpart4 = "</v3></triangle>" in
					
				fprintf oc "%s\n" top;  				(* write top *)
				fprintf oc "%s\n" vertexstart;  (* write start of vertices *)
				
				(* write vertices *)
				while (!line > 0) do
					if (!line > 4) then
						x := f1.x
					else 
						x := 0
					if (!line mod 4 = 0 || !line mod 4 = 3) then
						y := f1.y 
					else
						y := 0
					if (!line mod 2 = 0) then
						z := f1.z
					else 
						(z := 0)
					fprintf oc "%s%d%s%d%s%d%s\n" vpart1 x vpart2 y vpart3 z vpart4; (* write each vertex *) 
					!line = !line - 1;							
				done						
				
				fprintf oc "%s\n" vertexend;	(* write end of vertices *)
				
				fprintf oc "%s\n" trianglestart;	(* write start of triangles *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0) tpart2 !vertices.(1) tpart3 !vertices.(3) tpart4; (* write each triangle *) 
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0) tpart2 !vertices.(2) tpart3 !vertices.(3) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(5) tpart2 !vertices.(6) tpart3 !vertices.(7) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(4) tpart2 !vertices.(5) tpart3 !vertices.(7) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0) tpart2 !vertices.(1) tpart3 !vertices.(5) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0) tpart2 !vertices.(4) tpart3 !vertices.(5) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(2) tpart2 !vertices.(3) tpart3 !vertices.(7) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(2) tpart2 !vertices.(6) tpart3 !vertices.(7) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0) tpart2 !vertices.(2) tpart3 !vertices.(6) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0) tpart2 !vertices.(4) tpart3 !vertices.(6) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(1) tpart2 !vertices.(3) tpart3 !vertices.(7) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(1) tpart2 !vertices.(5) tpart3 !vertices.(7) tpart4; (* write each triangle *)				
				
				fprintf oc "%s\n" triangleend;	(* write end of triangles *)
					
				fprintf oc "%s\n" bottom;  			(* write bottom *)
				close_out oc                		(* flush and close the channel *)
	
	| _ -> ()
	


	
*)

let generate (globals, functions) =
	
  print_endline "Add generator code here ...\n"

	(*
	
let print (globals, functions) =
	if (stmt.Fr_print != "") then 
		
		
	
		let frame_list = List.fold_left (fun m (t, n) -> String.Map.add n t m)
	StringMap.empty (fr_decl.fr_name @ stmt.Fr_decl.frame_name)
	
	
	
	
	let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals )
	print_endline "Add generator code here ...\n"

if (hey != "") then print_string "yes" else print_string "no";;
	
*)	
	
(*
(* GENERATE AMF FILE CODE BELOW*)
(* Read in .blox file into a list *)
#load "str.cma"
	let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in loop [] ;;
(*	List.rev(read_lines "HelloWorld.blox");; *)

(* Convert list to string and "initialize variables"  *)
let string_file = String.concat "#" (List.rev(read_lines "HelloWorld.blox"))
let i = ref 0
let frame_list = ref " "
let frame_dim = ref " "
let frame_name = ref " "
let temp_name = ref " "
(* let frame_size = ref " " *)
let pairs = Hashtbl.create 10;;

(* Find frames and respective dimensions, store in hash map *)
try i := (Str.search_forward(Str.regexp "\\(Convert\\)[ A-Za-z]+") string_file 0);
	frame_name := Str.matched_string string_file;
	(* frame_size := String.length !frame_name; *)
	i := (String.length !frame_name) + !i;
	frame_name := String.trim(Str.string_after !(frame_name) 7);
	ignore(Str.search_forward(Str.regexp "\\(Frame\\)[ <>,0-9]*\\(base\\)") string_file 0);
	frame_dim := Str.matched_string string_file;
	ignore(Str.search_forward(Str.regexp "\\( \\)*[0-9,]+\\( \\)*") !frame_dim 0);
	frame_dim := Str.matched_string !frame_dim;
	Hashtbl.add pairs !frame_name !frame_dim;
with _ -> ();;	
(* with _ -> "Not printing to AMF";; *)

(* Check if string contains substring *)	
let contains s1 s2 =
	let re = Str.regexp_string s2 in
			try ignore (Str.search_forward re s1 0); true
			with Not_found -> false;;

(* Continuation of prior fxn, INCOMPLETE *)			
Str.search_forward(Str.regexp "\\(Join\\)[ A-Za-z0-9{}(),]*") string_file !i;;
temp_name := Str.matched_string string_file;
if (contains !temp_name !frame_name)
	then 
		frame_name := !temp_name;
		Str.search_forward(Str.regexp "\\(,\\)[A-Za-z ]*\\(,\\)") !frame_name 0;
		frame_name := Str.matched_string !frame_name;
		Str.search_forward(Str.regexp "[A-Za-z]") !frame_name 0;
		frame_name := Str.matched_string !frame_name;
		Str.search_forward(Str.regexp ("\\(Frame\\)[ <>,0-9]*"^"\\("^ "base" ^"\\)")) string_file !i;;
		frame_dim := Str.matched_string string_file;
						
(* Function to save frames and values to amf file *)					
open Printf
let write amf_file =						 	  				
	let oc = open_out amf_file in   	(* create or truncate file, return channel *)
			let head = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<amf>\n\t<object id=\"1\">\n\t\t<mesh>\n" in 
				let foot = "\n\t\t</mesh>\n\t</object>\n</amf>" in   
			fprintf oc "%s\n" head;  (* write head *) 
			Hashtbl.iter (fprintf oc "%s is %s.\n") pairs; 
		fprintf oc "%s\n" foot;  (* write foot *)
				close_out oc                		(* flush and close the channel *)
		(* normal exit: all channels are flushed and closed *);;

write "HelloWorld.amf";;



*)

					(*~~~~~~~~~~~~~~~~ STOP ~~~~~~~~~~~~~~~~*)
(*~~~~~~~~~~~~~~~~  PRIOR ITERATIONS BELOW ~~~~~~~~~~~~~~~~*)	
(* Rec function to find frames attached to printed frame and shift their values *)
(* #load "str.cma"

open Printf
let frame_list = ref " "
let read blox_file : string list =
	let ic = open_in blox_file in
		let read () = try Some (input_line ic) 
		              with End_of_file -> None in
			let rec loop acc = match read () with
				| Some s -> (try let _ = Str.search_forward (Str.regexp "Print") s 0 in
					frame_list := s; loop (s :: acc)
										 with _ -> (); loop (s :: acc))
				| None -> close_in ic; List.rev acc in loop [];;

ignore(read "HelloWorld.blox");;

(* Function to save frames and values to amf file *)
let write amf_file =						 	  				
	let oc = open_out amf_file in    	(* create or truncate file, return channel *)
		fprintf oc "%s\n" !frame_list;  (* write something *)   
		close_out oc                		(* flush and close the channel *)
(* normal exit: all channels are flushed and closed *);;

write "HelloWorld.amf";;

open Printf
  

let filetwo = "HelloWorld.blox"
let message = "Hello!"
  
let () =
	  (* Read file and display the first line *)
  let ic = open_in filetwo in
  try 
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    print_endline line;          (* write the result to stdout *)
    flush stdout;                (* write on the underlying device now *)
    close_in ic                  (* close the input channel *) 
  
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)
	let fileone = "HelloWorld.amf"																	
	let () =															
  (* Write message to file *)
  let oc = open_out fileone in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc                (* flush and close the channel *)
  
  (* normal exit: all channels are flushed and closed *);;
	
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in loop [] ;;
	
	read_lines "HelloWorld.blox";;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
	#load "str.cma";;
	let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> if (Str.string_match (Str.regexp "Frame") s 0) then (
									print_string("1010101010101" ^ s);
									loop (s :: acc))
								else loop (s :: acc)
    | None -> close_in ic; List.rev acc in loop [] in 
	
	read_lines "HelloWorld.blox";;
	
	
	read_lines "HelloWorld.blox";;
	
	
	Str.string_match (Str.regexp ^"Print") s 0) then (print_string "HERE" ^ s; 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
	#load "str.cma";;
	let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> (try
									(Str.search_forward (Str.regexp "Print") s 0) with 
								  | Not_found -> (); loop (s :: acc)
									| int ->	loop (s :: acc))

									
    | None -> close_in ic; List.rev acc in loop [] in 
	
	read_lines "HelloWorld.blox";;
	
	print_string (s ^ "10101010"); 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
(*KEEP*)

(* Rec function to find frames attached to printed frame and shift their values *)
#load "str.cma"

open Printf
let frame_list = ref " "
let read blox_file : string list =
	let ic = open_in blox_file in
		let read () = try Some (input_line ic) 
		              with End_of_file -> None in
			let rec loop acc = match read () with
				| Some s -> (try let _ = Str.search_forward (Str.regexp "Print") s 0 in
					frame_list := s; loop (s :: acc)
										 with _ -> (); loop (s :: acc))
				| None -> close_in ic; List.rev acc in loop [];;

ignore(read "HelloWorld.blox");;

(* Function to save frames and values to amf file *)
let write amf_file =						 	(* Write message to file *)					
	let oc = open_out amf_file in    	(* create or truncate file, return channel *)
		fprintf oc "%s\n" !frame_list;  (* write something *)   
		close_out oc                		(* flush and close the channel *)
(* normal exit: all channels are flushed and closed *);;

write "HelloWorld.amf";;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
let read_lines inc =
   let rec loop acc =
     match input_line inc  with
     | l -> loop (l :: acc)
     | exception End_of_file -> List.rev acc
   in
   loop []	
	
	
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
	
	let in_channel = open_in "HelloWorld.blox" in
try
  while true do
    let line = input_line in_channel in
    (* do something with line *)
  done
with End_of_file ->
  close_in in_channel
	
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
(* Rec function to find frames attached to printed frame and shift their values *)
#load "str.cma"

open Printf
let frame_list = ref " "
let read blox_file : string list =
	let ic = open_in blox_file in
		let read () = try Some (input_line ic) 
		              with End_of_file -> None in
			let rec loop acc = match read () with
				| Some s -> (try let _ = Str.search_forward (Str.regexp "Print") s 0 in
					frame_list := s; loop (s :: acc)
										 with _ -> (); loop (s :: acc))
				| None -> close_in ic; List.rev acc in loop [];;

ignore(read "HelloWorld.blox");;

(* Function to save frames and values to amf file *)
let write amf_file =						 	  				
	let oc = open_out amf_file in    	(* create or truncate file, return channel *)
		fprintf oc "%s\n" !frame_list;  (* write something *)   
		close_out oc                		(* flush and close the channel *)
(* normal exit: all channels are flushed and closed *);;

write "HelloWorld.amf";;
----------------------------------------------------------------------------------------
let ic = open_in "HelloWorld.blox" in
  Scanf.fscanf "%s %s %s %s\n" (fun value1 key1 value2 key2-> (value1, key1, value2, key2))

	
let ic = open_in "data.txt" in
  Scanf.fscanf "%d %d %s\n" (fun index value key -> (index, value, key))
----------------------------------------------------------------------------------------	
	
let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);;
	
	
	let in_channel = open_in "HelloWorld.blox" in
try
  Stream.iter
    (fun line ->
(*       do something with line *)
       print_endline line)
    (line_stream_of_channel in_channel);
  close_in in_channel
with e ->
  close_in in_channel;
  raise e;;
	
let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);;
----------------------------------------------------------------------------------------	
	
	let in_channel = open_in "HelloWorld.blox" in
try
  Stream.iter
    (fun line ->
(*       do something with line *)
       print_endline line)
    (line_stream_of_channel in_channel);
  close_in in_channel
with e ->
  close_in in_channel;
  raise e;;


#load "str.cma";;
let line_stream_of_string string =
    Stream.of_list (Str.split (Str.regexp "\n") string);;
	
	let filename = "HelloWorld.blox"
	 let process_line line =
    print_endline line
  let process_lines lines =
    Stream.iter process_line lines
  let process_file filename =
    let in_channel = open_in filename in
    try
      process_lines (line_stream_of_channel in_channel);
      close_in in_channel
    with e ->
      close_in in_channel;
      raise e
  let process_string string =
    process_lines (line_stream_of_string string);;
		
		
let hash_of_stream stream =
    let result = Hashtbl.create 0 in
    Stream.iter
      (fun (key, value) -> Hashtbl.replace result key value)
		
      stream;
    result;;		
	let in_channel = open_in "HelloWorld.blox" in

  hash_of_stream
    (fun line ->
(*       ignore(hash_of_stream line); *)
     ) in
    (line_stream_of_channel in_channel);
  close_in in_channel
;;		
*)
