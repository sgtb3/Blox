open Ast
open Printf

exception Face_Taken of string;;
exception Block_Overlap of string;;
exception Invalid_Face of string;;
exception Opposite_Face of string;;
exception Invalid_Block of string;;
exception Block_Out_Of_Bounds of string;;

let generate frm =
    let name = frm.fr_id ^ ".amf" in
    let oc = open_out name in
      let x = ref 0 in
      let y = ref 0 in
      let z = ref 0 in
      let z_val = ref 0 in
      let y_val = ref 0 in
      let x_val = ref 0 in
      let vertices = ref [|0; 1; 2; 3; 4; 5; 6; 7|] in
      let line = ref 8 in
      let top = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<amf> \
                \n\t<object id=\"1\">\n\t\t<mesh>" in
      let bottom = "\t\t</mesh>\n\t</object>\n</amf>" in
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

      let check = ref 0 in
      let displacement = ref 0 in
      let actlength = ref 0 in
      let length = ref (Array.length frm.blocks) in

      fprintf oc "%s\n" top;
      fprintf oc "%s\n" vertexstart;

      while (!check < !length) do(
        if ((Array.length frm.blocks.(!check).faces) = 6) then(
          z_val := (!check mod frm.z);
          y_val := (((!check - !z_val) / frm.z) mod frm.y);
          x_val := ((((!check - !z_val) / frm.z) - !y_val) / frm.y);

            while (!line > 0) do(
              if (!line > 4) then
                x := !x_val
              else(
                x := !x_val - 1
              );
              if (!line mod 4 = 0 || !line mod 4 = 3) then
                y := !y_val
              else(
                y := !y_val - 1
              );
              if (!line mod 2 = 0) then
                z := !z_val
              else(
                z := !z_val - 1
              );
              fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
                vpart1 !x vpart2 !y vpart3 !z vpart4;
              line := !line - 1
            )done;
              check := !check + 1;
              actlength := !actlength + 1;
              line := 8
          )
          else(
            check := !check + 1
          )
      )done;

      fprintf oc "%s\n" vertexend;
      fprintf oc "%s\n" trianglestart;
      check := 0;

      while (!actlength > !check) do(
        displacement := !check * 8;
        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(0) + !displacement)
          tpart2 (!vertices.(1) + !displacement)
          tpart3 (!vertices.(3) + !displacement)
          tpart4; (* write each triangle *)
        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(0) + !displacement)
          tpart2 (!vertices.(2) + !displacement)
          tpart3 (!vertices.(3) + !displacement)
          tpart4; (* write each triangle *)

        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(5) + !displacement)
          tpart2 (!vertices.(6) + !displacement)
          tpart3 (!vertices.(7) + !displacement)
          tpart4; (* write each triangle *)
        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(4) + !displacement)
          tpart2 (!vertices.(5) + !displacement)
          tpart3 (!vertices.(6) + !displacement)
          tpart4; (* write each triangle *)

        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(0) + !displacement)
          tpart2 (!vertices.(1) + !displacement)
          tpart3 (!vertices.(5) + !displacement)
          tpart4; (* write each triangle *)
        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(0) + !displacement)
          tpart2 (!vertices.(4) + !displacement)
          tpart3 (!vertices.(5) + !displacement)
          tpart4; (* write each triangle *)

        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(2) + !displacement)
          tpart2 (!vertices.(3) + !displacement)
          tpart3 (!vertices.(7) + !displacement)
          tpart4; (* write each triangle *)
        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(2) + !displacement)
          tpart2 (!vertices.(6) + !displacement)
          tpart3 (!vertices.(7) + !displacement)
          tpart4; (* write each triangle *)

        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(0) + !displacement)
          tpart2 (!vertices.(2) + !displacement)
          tpart3 (!vertices.(6) + !displacement)
          tpart4; (* write each triangle *)
        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(0) + !displacement)
          tpart2 (!vertices.(4) + !displacement)
          tpart3 (!vertices.(6) + !displacement)
          tpart4; (* write each triangle *)

        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(1) + !displacement)
          tpart2 (!vertices.(3) + !displacement)
          tpart3 (!vertices.(7) + !displacement)
          tpart4; (* write each triangle *)
        fprintf oc "\t\t\t\t%s%d%s%d%s%d%s\n"
          tpart1 (!vertices.(1) + !displacement)
          tpart2 (!vertices.(5) + !displacement)
          tpart3 (!vertices.(7) + !displacement)
          tpart4; (* write each triangle *)

        check := !check + 1;
      )done;

      fprintf oc "%s\n" triangleend;
      fprintf oc "%s\n" bottom;
      close_out oc;;