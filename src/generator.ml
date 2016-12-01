

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


