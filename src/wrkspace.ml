(* 1D ARAY CODE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

(* Declaration examples
let blck1 = {faces = [|true;true;true;true;true;true|]};;

let arr = Array.init 3 (fun _ -> {faces = [|true;true;true;true;true;true|]});;

let drr = Array.init 3 (fun i -> {faces = Array.copy arr.(i).(j).(k).faces});;

let frm = {x = 3; y = 3; z = 3; blocks = arr}
*)


exception Face_Taken of string;;
exception Block_Overlap of string;;
exception Invalid_Face of string;;
exception Opposite_Face of string;;
exception Invalid_Block of string;;
exception Block_Out_Of_Bounds of string;;


type blck = {
  faces : bool array;
}


type frame = {
  x       : int;
  y       : int;
  z       : int;
  blocks  : blck array;
}


(* faceCheck takes a 1D array and it's 3D dimensions *)
let faceCheck a x y z=

  let gx elx =
    if Array.length elx.faces = 6 then(
      Array.set elx.faces 1 false;
      false)
    else true in
  let gy ely =
    if Array.length ely.faces = 6 then(
      Array.set ely.faces 3 false;
      false)
    else true in
  let gz elz =
    if Array.length elz.faces = 6 then(
      Array.set elz.faces 5 false;
      false)
    else true in

  let f i el =
    if Array.length el.faces = 6 then(
      Array.set el.faces 0 (if ((i + (y*z)) < (x*y*z)) then(
        gx(a.(i + y*z)))
      else true;);
      Array.set el.faces 2 (if (((i mod (y*z)) + z) < (y*z)) then(
        gy(a.(i + z)))
      else true;);
      Array.set el.faces 4 (if (((i mod z) + 1) < z) then(
        gz(a.(i + 1)))
      else true))
  else ignore() in

  Array.iteri f a
;;


let argCheck frameA ax ay az af frameB bx by bz bf =

  let aArray = Array.init (Array.length frameA.blocks) (fun i -> {faces = Array.copy frameA.blocks.(i).faces}) in
  let ai = ((((ax * frameA.y) + ay) * frameA.z) + az) in

  let bArray = Array.init (Array.length frameB.blocks) (fun i -> {faces = Array.copy frameB.blocks.(i).faces}) in
  let bi = ((((bx * frameB.y) + by) * frameB.z) + bz) in

  (* Check specified blocks are within array boundaries *)
  if (ax >= frameA.x) || (ay >= frameA.y) || (az >= frameA.z)
    then raise (Block_Out_Of_Bounds "Specified block for first frame is outside array boundaries")
  else ignore();
  if (bx >= frameB.x) || (by >= frameB.y) || (bz >= frameB.z)
    then raise (Block_Out_Of_Bounds "Specified block for second frame is outside array boundaries")
  else ignore();

  (* Check given block exists *)
  if not (Array.length frameA.blocks.(ai).faces = 6)
    then raise (Invalid_Block "Specified Block for first frame does not exist")
  else ignore();
  if not (Array.length frameB.blocks.(bi).faces = 6)
    then raise (Invalid_Block "Specified Block for second frame does not exist")
  else ignore();

  (* Check for valid faces *)
  if (af = "E") || (af = "W") || (af = "N") || (af = "S") || (af = "F") || (af = "B")
    then ignore()
  else raise (Invalid_Face "Specified face for first frame must be E, W, N, S, F, or B");
  if (bf = "E") || (bf = "W") || (bf = "N") || (bf = "S") || (bf = "F") || (bf = "B")
    then ignore()
  else raise (Invalid_Face "Specified face for second frame must be E, W, N, S, F, or B");

  let aface =
    (if af = "E" then
      aArray.(ai).faces.(0)
    else if af = "W" then
      aArray.(ai).faces.(1)
    else if af = "N" then 
      aArray.(ai).faces.(2)
    else if af = "S" then 
      aArray.(ai).faces.(3)
    else if af = "F" then 
      aArray.(ai).faces.(4)
    else if af = "B" then 
      aArray.(ai).faces.(5)
    else false) in

  let (bface, bx_shift, by_shift, bz_shift) =
    (if bf = "E" then (bArray.(bi).faces.(0), (ax - 1) - bx, ay - by, az - bz)
    else if bf = "W" then(bArray.(bi).faces.(1), (ax + 1) - bx, ay - by, az - bz)
    else if bf = "N" then(bArray.(bi).faces.(2), ax - bx, (ay - 1) - by, az - bz)
    else if bf = "S" then(bArray.(bi).faces.(3), ax - bx, (ay + 1) - by, az - bz) 
    else if bf = "F" then(bArray.(bi).faces.(4), ax - bx, ay - by, (az - 1) - bz)
    else if bf = "B" then(bArray.(bi).faces.(5), ax - bx, ay - by, (az + 1) - bz)
    else (false, 0, 0, 0)) in
  
  (* check if frameA's block face is available *)
  if not(aface) then
    raise (Face_Taken "Specified face of block in first frame is unavailable")
  else ignore();
  
  (* check if frameB's block face is available *)
  if not(bface) then 
    raise (Face_Taken "Specified face of block in second frame is unavailable")
  else ignore();
  
  (* check for opposite faces *)
  if (((af = "E") && not(bf = "W")) ||
      ((af = "W") && not(bf = "E"))) then 
    raise (Opposite_Face "Must specify opposite faces")
  else ignore();

  if (((af = "N") && not(bf = "S")) ||
      ((af = "S") && not(bf = "N"))) then 
    raise (Opposite_Face "Must specify opposite faces")
  else ignore();
  
  if (((af = "F") && not(bf = "B")) ||
      ((af = "B") && not(bf = "F"))) then 
    raise (Opposite_Face "Must specify opposite faces")
  else ignore();

  (* Determine shift values for A and B *)
  let (ax_shift, bx_shift) =
    (if bx_shift < 0 then (-bx_shift, 0) else (0, bx_shift)) in

  let (ay_shift, by_shift) =
    (if by_shift < 0 then (-by_shift, 0) else (0, by_shift)) in

  let (az_shift, bz_shift) =
    (if bz_shift < 0 then (-bz_shift, 0) else (0, bz_shift)) in

  (* Return shift values and copied arrays *)
  (ax_shift, ay_shift, az_shift, bx_shift, by_shift, bz_shift, aArray, bArray)
;;



let join frameA ax ay az af frameB bx by bz bf =

  let (ax_shift, ay_shift, az_shift, bx_shift, by_shift, bz_shift, aArray, bArray) = argCheck frameA ax ay az af frameB bx by bz bf in

  (* Determine size of new array *)
  let cx = (max (frameA.x + ax_shift) (frameB.x + bx_shift)) in
  let cy = (max (frameA.y + ay_shift) (frameB.y + by_shift)) in
  let cz = (max (frameA.z + az_shift) (frameB.z + bz_shift)) in

  (* Create new array of blocks *)
  let c = Array.init (cx * cy * cz) (fun _ -> let b = {faces = [||] } in b ) in

  (* Fill c with blocks from array A *)
  let f i el =
    if Array.length el.faces = 6 then(
      let z_val = (i mod frameA.z) in
      let y_val = (((i - z_val) / frameA.z) mod frameA.y) in
      let x_val = ((((i - z_val) / frameA.z) - y_val) / frameA.y) in
      let cz_val = z_val + az_shift in
      let cy_val = y_val + ay_shift in
      let cx_val = x_val + ax_shift in
      let ci = ((((cx_val * cy) + cy_val) * cz) + cz_val) in
      Array.set c ci el)
    else ignore() in
  Array.iteri f aArray;

  (* Fill c with blocks from array B *)
  let g i el =
    if Array.length el.faces = 6 then(
      let z_val = (i mod frameB.z) in
      let y_val = (((i - z_val) / frameB.z) mod frameB.y) in
      let x_val = ((((i - z_val) / frameB.z) - y_val) / frameB.y) in
      let cz_val = z_val + bz_shift in
      let cy_val = y_val + by_shift in
      let cx_val = x_val + bx_shift in
      let ci = ((((cx_val * cy) + cy_val) * cz) + cz_val) in
      if (Array.length c.(ci).faces = 0) then(
        Array.set c ci el)
      else raise (Block_Overlap "The specified join causes overlap"))
    else ignore() in
  Array.iteri g bArray;

  (* Run faceCheck *)
  faceCheck c cx cy cz;

  (* Update Frame A with Cblocks array to finish merge of B into A *)
  let frameC = {
  x = cx;
  y = cy;
  z = cz; 
  blocks = c;
  } in

  frameC
;;