

(* faceCheck function *)
let faceCheck a = 

  for i = 0 to Array.length a - 1 do

    for j = 0 to Array.length a.(i) - 1 do

      for k = 0 to Array.length a.(i).(j) - 1 do
        let b = a.(i).(j).(k) in

        if Array.length b.faces = 6 then(

          let xblck = (if i < Array.length a - 1 then a.(i + 1).(j).(k) else {faces = [||]}) in
          let yblck = (if j < Array.length a.(i) - 1 then a.(i).(j + 1).(k) else {faces = [||]}) in
          let zblck = (if k < Array.length a.(i).(j) - 1 then a.(i).(j).(k + 1) else {faces = [||]}) in

          if Array.length xblck.faces = 6 then(
            Array.set b.faces 0 false;
            Array.set xblck.faces 1 false)
          else ignore();

          if Array.length yblck.faces = 6 then(
            Array.set b.faces 2 false;
            Array.set yblck.faces 3 false)
          else ignore();

          if Array.length zblck.faces = 6 then(
            Array.set b.faces 4 false;
            Array.set zblck.faces 5 false)
          else ignore())

        else ignore();

      done;
    done;
  done;


(* Helper function for Join*)
let frm_mod xarg yarg zarg blcksarg =
  let f = {x = xarg; y = yarg; z = zarg; blocks = blcksarg} in f


(* Join Function*)
let join frameA a b c d frameB e f g h = 
  let ax = a in 
  let ay = b in
  let az = c in
  let afacetr = d in
  
  let bx = e in
  let by = f in
  let bz = g in
  let bfacetr = h in
  

  if frameA = frameB then prerr_string "Error: Attempting to join blocks from the same Frame."
  else ignore();
      
  let aface =
    (if afacetr = "E" then
      frameA.blocks.(ax).(ay).(az).faces.(0)
    else if afacetr = "W" then
      frameA.blocks.(ax).(ay).(az).faces.(1)
    else if afacetr = "N" then 
      frameA.blocks.(ax).(ay).(az).faces.(2)
    else if afacetr = "S" then 
      frameA.blocks.(ax).(ay).(az).faces.(3)
    else if afacetr = "F" then 
      frameA.blocks.(ax).(ay).(az).faces.(4)
    else if afacetr = "B" then 
      frameA.blocks.(ax).(ay).(az).faces.(5)
    else false) in

  let (bface, bx_shift, by_shift, bz_shift) =
    (if bfacetr = "E" then (frameB.blocks.(ax).(ay).(az).faces.(0), (ax - 1) - bx, ay - by, az - bz)
    else if bfacetr = "W" then(frameB.blocks.(ax).(ay).(az).faces.(1), (ax + 1) - bx, ay - by, az - bz)
    else if bfacetr = "N" then(frameB.blocks.(ax).(ay).(az).faces.(2), ax - bx, (ay - 1) - by, az - bz)
    else if bfacetr = "S" then(frameB.blocks.(ax).(ay).(az).faces.(3), ax - bx, (ay + 1) - by, az - bz) 
    else if bfacetr = "F" then(frameB.blocks.(ax).(ay).(az).faces.(4), ax - bx, ay - by, (az - 1) - bz)
    else if bfacetr = "B" then(frameB.blocks.(ax).(ay).(az).faces.(5), ax - bx, ay - by, (az + 1) - bz)
    else (false, 0, 0, 0)) in
  
  (* check if frameA's block face is available *)
  if not(aface) then 
    prerr_string "Error: Block face is not available for Join with"
  else ignore();
  
  (* check if frameB's block face is available *)
  if not(bface) then 
    prerr_string "Error: Block face is not available for Join with"
  else ignore();
  
  (* check for opposite faces *)
  if (((afacetr = "E") && not(bfacetr = "W")) ||
      ((afacetr = "W") && not(bfacetr = "E"))) then 
    prerr_string "Error: Illegal face option."
  else ignore();

  if (((afacetr = "N") && not(bfacetr = "S")) ||
      ((afacetr = "S") && not(bfacetr = "N"))) then 
    prerr_string "Error: Illegal face option."
  else ignore();
  
  if (((afacetr = "F") && not(bfacetr = "B")) ||
      ((afacetr = "B") && not(bfacetr = "F"))) then 
    prerr_string "Error: Illegal face option."
  else ignore();

  (*  ========== ALL CHECKS PASSED. BEGIN JOIN PROCESS ========== *)

  (* Determine shift values for A and B *)
  let (ax_shift, bx_shift) =
    (if bx_shift < 0 then (-bx_shift, 0) else (0, bx_shift)) in

  let (ay_shift, by_shift) =
    (if by_shift < 0 then (-by_shift, 0) else (0, by_shift)) in

  let (az_shift, bz_shift) =
    (if bz_shift < 0 then (-bz_shift, 0) else (0, bz_shift)) in
  

  (* Determine size of new array *)
  let cx_max = (max (frameA.x + ax_shift) (frameB.x + bx_shift)) in
  let cy_max = (max (frameA.y + ax_shift) (frameB.y + by_shift)) in
  let cz_max = (max (frameA.z + ax_shift) (frameB.z + bz_shift)) in


  (* Create new array of blocks *)
  let c = Array.init cx_max (fun _ -> Array.init cy_max (fun _ -> (Array.init cz_max 
    (fun _ -> let b = {faces = [||] } in b )))) in

  (* Fill c with blocks from array A *)
  for i = 0 to frameA.x - 1 do

    for j = 0 to frameA.y - 1 do

      for k = 0 to frameA.z - 1 do

        let b = (Array.get (Array.get (Array.get frameA.blocks i) j) k) in
        if Array.length b.faces = 6 then (Array.set (Array.get (Array.get c i) j) k b)
        else ignore();

      done;
    done;
  done;


  (* Fill c with blocks from array B *)
  for i = 0 to frameB.x - 1 do

    for j = 0 to frameB.y - 1 do

      for k = 0 to frameB.z - 1 do

        let b = (Array.get (Array.get (Array.get frameB.blocks i) j) k) in
        if Array.length b.faces = 6 then (Array.set (Array.get (Array.get c i) j) k b)
        else ignore();

      done;
    done;
  done;


  (* Update Frame A with Cblocks array to finish merge of B into A *)
  let frameA = frm_mod cx_max cy_max cz_max c in

  (* Run faceCheck *)
  faceCheck frameA.blocks

	
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

