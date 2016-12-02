(* Declaration examples
let blck1 = {faces = [|true;true;true;true;true;true|]};;

let arr = Array.init 3 (fun _ -> Array.init 3 (fun _ -> (Array.init 3 (fun _ -> {faces = [|true;true;true;true;true;true|]}))));;

let drr = Array.init 3 (fun i -> Array.init 3 (fun j -> (Array.init 3 (fun k -> {faces = Array.copy arr.(i).(j).(k).faces}))));;

Array.set (Array.get (Array.get (Array.get drr 0) 0) 0).faces 0 false;;

let frm = {x = 3; y = 3; z = 3; blocks = arr}
*)


exception Face_Taken of string;;
exception Block_Overlap of string;;


type blck = {
  faces : bool array;
}


type frame = {
  x       : int;
  y       : int;
  z       : int;
  blocks  : blck array array array;
}


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
;;


let join frameA a b c d frameB e f g h =

  let aArray = Array.init frameA.x (fun i -> Array.init frameA.y (fun j -> (Array.init frameA.z (fun k -> {faces = Array.copy frameA.blocks.(i).(j).(k).faces})))) in
  let ax = a in 
  let ay = b in
  let az = c in
  let afacetr = d in
  
  let bArray = Array.init frameB.x (fun i -> Array.init frameB.y (fun j -> (Array.init frameB.z (fun k -> {faces = Array.copy frameB.blocks.(i).(j).(k).faces})))) in
  let bx = e in
  let by = f in
  let bz = g in
  let bfacetr = h in
  
(*)
  if frameA = frameB then prerr_string "Error: Attempting to join blocks from the same Frame."
  else ignore(); *)
      
  let aface =
    (if afacetr = "E" then
      aArray.(ax).(ay).(az).faces.(0)
    else if afacetr = "W" then
      aArray.(ax).(ay).(az).faces.(1)
    else if afacetr = "N" then 
      aArray.(ax).(ay).(az).faces.(2)
    else if afacetr = "S" then 
      aArray.(ax).(ay).(az).faces.(3)
    else if afacetr = "F" then 
      aArray.(ax).(ay).(az).faces.(4)
    else if afacetr = "B" then 
      aArray.(ax).(ay).(az).faces.(5)
    else false) in

  let (bface, bx_shift, by_shift, bz_shift) =
    (if bfacetr = "E" then (bArray.(bx).(by).(bz).faces.(0), (ax - 1) - bx, ay - by, az - bz)
    else if bfacetr = "W" then(bArray.(bx).(by).(bz).faces.(1), (ax + 1) - bx, ay - by, az - bz)
    else if bfacetr = "N" then(bArray.(bx).(by).(bz).faces.(2), ax - bx, (ay - 1) - by, az - bz)
    else if bfacetr = "S" then(bArray.(bx).(by).(bz).faces.(3), ax - bx, (ay + 1) - by, az - bz) 
    else if bfacetr = "F" then(bArray.(bx).(by).(bz).faces.(4), ax - bx, ay - by, (az - 1) - bz)
    else if bfacetr = "B" then(bArray.(bx).(by).(bz).faces.(5), ax - bx, ay - by, (az + 1) - bz)
    else (false, 0, 0, 0)) in
  
  (* check if frameA's block face is available *)
  if not(aface) then
    raise (Face_Taken "Specified face of block in frame A is already taken")
  else ignore();
  
  (* check if frameB's block face is available *)
  if not(bface) then 
    raise (Face_Taken "Specified face of block in frame B is already taken")
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
  let cy_max = (max (frameA.y + ay_shift) (frameB.y + by_shift)) in
  let cz_max = (max (frameA.z + az_shift) (frameB.z + bz_shift)) in


  (* Create new array of blocks *)
  let c = Array.init cx_max (fun _ -> Array.init cy_max (fun _ -> (Array.init cz_max 
    (fun _ -> let b = {faces = [||] } in b )))) in

  (* Fill c with blocks from array A *)
  for i = 0 to frameA.x - 1 do

    for j = 0 to frameA.y- 1 do

      for k = 0 to frameA.z - 1 do

        let b = (Array.get (Array.get (Array.get aArray i) j) k) in
        if Array.length b.faces = 6 then
          (Array.set (Array.get (Array.get c (i + ax_shift)) (j + ay_shift)) (k + az_shift) b)
        else ignore();

      done;
    done;
  done;


  (* Fill c with blocks from array B *)
  for i = 0 to frameB.x - 1 do

    for j = 0 to frameB.y - 1 do

      for k = 0 to frameB.z - 1 do

        let bb = (Array.get (Array.get (Array.get bArray i) j) k) in
        if Array.length bb.faces = 6 then(
          if Array.length (Array.get (Array.get (Array.get c (i + bx_shift)) (j + by_shift)) (k + bz_shift)).faces = 0 then
            (Array.set (Array.get (Array.get c (i + ax_shift)) (j + ay_shift)) (k + az_shift) bb)
          else raise(Block_Overlap "The specified join causes overlap"))
        else ignore();

      done;
    done;
  done;


  (* Run faceCheck *)
  faceCheck c;


  (* Update Frame A with Cblocks array to finish merge of B into A *)
  let frameC = {
  x = cx_max;
  y = cy_max;
  z = cz_max; 
  blocks = c;
  } in

  frameC
;;

