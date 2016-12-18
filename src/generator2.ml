open Ast
open Sast
open Printf

exception Face_Taken of string;;
exception Block_Overlap of string;;
exception Invalid_Face of string;;
exception Opposite_Face of string;;
exception Invalid_Block of string;;
exception Block_Out_Of_Bounds of string;;

let x = ref 0 in
let y = ref 0 in 
let z = ref 0 in
let vertices = ref [|0; 1; 2; 3; 4; 5; 6; 7|] in
let line = ref 8 in
let generate = function 
	| Fr_print frm -> 
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
				
				Array.iteri (fun i m -> let z_val = (i mod frm.z) in
																let y_val = (((i - z_val) / frm.z) mod frm.y) in
																let x_val = ((((i - z_val) / frm.z) - y_val) / frm.y) in
																		
					(* write vertices *)
					while (!line > 0) do
						if (!line > 4) then
							x := x_val
						else 
							x := x_val - 1
						if (!line mod 4 = 0 || !line mod 4 = 3) then
							y := y_val 
						else
							y := y_val - 1
						if (!line mod 2 = 0) then
							z := z_val
						else 
							(z := z_val - 1)
						fprintf oc "%s%d%s%d%s%d%s\n" vpart1 x vpart2 y vpart3 z vpart4; (* write each vertex *) 
						!line = !line - 1;							
					done) frm.blocks; 						
				
				fprintf oc "%s\n" vertexend;	(* write end of vertices *)
				
				fprintf oc "%s\n" trianglestart;	(* write start of triangles *)
				
				let length = ref Array.length frm.blocks in
				let check = ref 0 in
				let displacement = ref 8 
				while (length > check) do					(* write triangles *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0+(check*displacement)) tpart2 !vertices.(1+(check*displacement)) tpart3 !vertices.(3+(check*displacement)) tpart4; (* write each triangle *) 
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0+(check*displacement)) tpart2 !vertices.(2+(check*displacement)) tpart3 !vertices.(3+(check*displacement)) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(5+(check*displacement)) tpart2 !vertices.(6+(check*displacement)) tpart3 !vertices.(7+(check*displacement)) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(4+(check*displacement)) tpart2 !vertices.(5+(check*displacement)) tpart3 !vertices.(7+(check*displacement)) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0+(check*displacement)) tpart2 !vertices.(1+(check*displacement)) tpart3 !vertices.(5+(check*displacement)) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0+(check*displacement)) tpart2 !vertices.(4+(check*displacement)) tpart3 !vertices.(5+(check*displacement)) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(2+(check*displacement)) tpart2 !vertices.(3+(check*displacement)) tpart3 !vertices.(7+(check*displacement)) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(2+(check*displacement)) tpart2 !vertices.(6+(check*displacement)) tpart3 !vertices.(7+(check*displacement)) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0+(check*displacement)) tpart2 !vertices.(2+(check*displacement)) tpart3 !vertices.(6+(check*displacement)) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(0+(check*displacement)) tpart2 !vertices.(4+(check*displacement)) tpart3 !vertices.(6+(check*displacement)) tpart4; (* write each triangle *)
				
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(1+(check*displacement)) tpart2 !vertices.(3+(check*displacement)) tpart3 !vertices.(7+(check*displacement)) tpart4; (* write each triangle *)
				fprintf oc "%s%d%s%d%s%d%s\n" tpart1 !vertices.(1+(check*displacement)) tpart2 !vertices.(5+(check*displacement)) tpart3 !vertices.(7+(check*displacement)) tpart4; (* write each triangle *)	
				
				check := check + 1
				done;
				
				fprintf oc "%s\n" triangleend;	(* write end of triangles *)
					
				fprintf oc "%s\n" bottom;  			(* write bottom *)
				close_out oc                		(* flush and close the channel *)
	
	| _ -> ()