
open Ast
open Sast

exception Face_Taken of string;;
exception Block_Overlap of string;;
exception Invalid_Face of string;;
exception Opposite_Face of string;;
exception Invalid_Block of string;;
exception Block_Out_Of_Bounds of string;;


let generate (globals, functions) =
  print_endline "Add generator code here ...\n"

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
