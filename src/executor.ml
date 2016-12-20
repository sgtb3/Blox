
open Ast
open Sast


exception Face_Taken of string;;
exception Block_Overlap of string;;
exception Invalid_Face of string;;
exception Opposite_Face of string;;
exception Invalid_Block of string;;
exception Block_Out_Of_Bounds of string;;


(* Return a frame with the given dimensions *)
let faceCons x y z w n =
  let fc = {dim = (x, y, z); face = w; fc_id = n} in
  fc;;

(* Takes 1D index and corresponding frame and returns 3D coordinates*)
let getCoord i frm =
  let z_val = (i mod frm.z) in
  let y_val = (((i - z_val) / frm.z) mod frm.y) in
  let x_val = ((((i - z_val) / frm.z) - y_val) / frm.y) in
  (x_val, y_val, z_val);;


(* Takes face array index returns face string *)
let getFcStr i = match i with
  0 -> "E"
| 1 -> "W"
| 2 -> "N"
| 3 -> "S"
| 4 -> "F"
| 5 -> "B"
| _ -> raise (Invalid_Face "Face index out of bounds");;


(* faceCheck takes a 1D array of blocks and its 3D dimensions, it updates block
  faces as unavailable if they are joined to another block *)
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
      else true)) in

  Array.iteri f a;;


(* Checks arguments given to join function and returns coordinate shifts *)
let argCheck frameA fidA frameB fidB =

  let (ax, ay, az) = fidA.dim in
  let (bx, by, bz) = fidB.dim in
  let af = fidA.face in
  let bf = fidB.face in
  let aArray = Array.init (Array.length frameA.blocks) (fun i -> {faces = Array.copy frameA.blocks.(i).faces}) in
  let ai = ((((ax * frameA.y) + ay) * frameA.z) + az) in
  let bArray = Array.init (Array.length frameB.blocks) (fun i -> {faces = Array.copy frameB.blocks.(i).faces}) in
  let bi = ((((bx * frameB.y) + by) * frameB.z) + bz) in

  (* Check specified blocks are within array boundaries *)
  if (ax >= frameA.x) || (ay >= frameA.y) || (az >= frameA.z)
    then raise (Block_Out_Of_Bounds "Specified block for first frame is outside array boundaries");
  if (bx >= frameB.x) || (by >= frameB.y) || (bz >= frameB.z)
    then raise (Block_Out_Of_Bounds "Specified block for second frame is outside array boundaries");

  (* Check given block exists *)
  if not (Array.length frameA.blocks.(ai).faces = 6)
    then raise (Invalid_Block "Specified Block for first frame does not exist");
  if not (Array.length frameB.blocks.(bi).faces = 6)
    then raise (Invalid_Block "Specified Block for second frame does not exist");

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
    raise (Face_Taken "Specified face of block in first frame is unavailable");
  
  (* check if frameB's block face is available *)
  if not(bface) then 
    raise (Face_Taken "Specified face of block in second frame is unavailable");
  
  (* check for opposite faces *)
  if (((af = "E") && not(bf = "W")) ||
      ((af = "W") && not(bf = "E"))) then 
    raise (Opposite_Face "Must specify opposite faces");
  if (((af = "N") && not(bf = "S")) ||
      ((af = "S") && not(bf = "N"))) then 
    raise (Opposite_Face "Must specify opposite faces");
  if (((af = "F") && not(bf = "B")) ||
      ((af = "B") && not(bf = "F"))) then 
    raise (Opposite_Face "Must specify opposite faces");

  (* Determine shift values for A and B *)
  let (ax_shift, bx_shift) =
    (if bx_shift < 0 then (-bx_shift, 0) else (0, bx_shift)) in
  let (ay_shift, by_shift) =
    (if by_shift < 0 then (-by_shift, 0) else (0, by_shift)) in
  let (az_shift, bz_shift) =
    (if bz_shift < 0 then (-bz_shift, 0) else (0, bz_shift)) in

  (* Return shift values and copied arrays *)
  (ax_shift, ay_shift, az_shift, bx_shift, by_shift, bz_shift, aArray, bArray);;


(* Joins two frames *)
let join frameA fidA frameB fidB =

  let (ax_shift, ay_shift, az_shift, bx_shift, by_shift, bz_shift, aArray, bArray) = argCheck frameA fidA frameB fidB in

  (* Determine size of new array *)
  let cx = (max (frameA.x + ax_shift) (frameB.x + bx_shift)) in
  let cy = (max (frameA.y + ay_shift) (frameB.y + by_shift)) in
  let cz = (max (frameA.z + az_shift) (frameB.z + bz_shift)) in

  (* Create new array of blocks *)
  let c = Array.init (cx * cy * cz) (fun _ -> let b = {faces = [||]} in b ) in

  (* Fill c with blocks from array A *)
  let f i el =
    if Array.length el.faces = 6 then(
      let (x_val, y_val, z_val) = getCoord i frameA in
      let cz_val = z_val + az_shift in
      let cy_val = y_val + ay_shift in
      let cx_val = x_val + ax_shift in
      let ci = ((((cx_val * cy) + cy_val) * cz) + cz_val) in
      Array.set c ci el) in
  Array.iteri f aArray;

  (* Fill c with blocks from array B *)
  let g i el =
    if Array.length el.faces = 6 then(
      let (x_val, y_val, z_val) = getCoord i frameB in
      let cz_val = z_val + bz_shift in
      let cy_val = y_val + by_shift in
      let cx_val = x_val + bx_shift in
      let ci = ((((cx_val * cy) + cy_val) * cz) + cz_val) in
      if (Array.length c.(ci).faces = 0) then(
        Array.set c ci el)
      else raise (Block_Overlap "The specified join causes overlap")) in
  Array.iteri g bArray;

  (* Run faceCheck *)
  faceCheck c cx cy cz;

  (* Create and return resulting frame C *)
  let frameC = {
  x = cx;
  y = cy;
  z = cz; 
  blocks = c;
  fr_id = "Result";
  } in

  frameC;;


(* build takes two frames and an array of face ID's for each frame, returns
   all possible frames made by joining the two original frames at the specified
   faces. If the faceID array is empty for either frame the algorithm assumes
   all open faces as possible join locations *)
let build frameA faceArrA frameB faceArrB =

  (* Create an array large enough to hold the maximum number of possible results *)
  let returnArr = match (Array.length faceArrA, Array.length faceArrB) with
      (0, 0)  ->  Array.init (6*(Array.length frameA.blocks)*(Array.length frameB.blocks)) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""})
    | (a, 0)  ->  Array.init (a*(Array.length frameA.blocks)) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""})
    | (0, b)  ->  Array.init (b*(Array.length frameB.blocks)) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""})
    | (a, b)  ->  Array.init (a*b) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}) in

  (* Return all faces in fLB that can be joined to face el *)
  let fndFcsB el fLB = match el.face with
    | "E" -> Array.map (fun x -> if (x.face = "W") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "W" -> Array.map (fun x -> if (x.face = "E") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "N" -> Array.map (fun x -> if (x.face = "S") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "S" -> Array.map (fun x -> if (x.face = "N") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "F" -> Array.map (fun x -> if (x.face = "B") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "B" -> Array.map (fun x -> if (x.face = "F") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    |  _  -> raise (Invalid_Face "A given face string for the first frame is not formated as one of: E, W, N, S, F, B") in

  (* Return all available faces in frm *)
  let allFc frm =
    let allFcArr = Array.init ((Array.length frm.blocks) * 6) (fun _ -> {dim = (0,0,0); face = "Empty"; fc_id = ""}) in
    let count = ref 0 in
    let finder1 i el =
      let finder2 j fel =
        if fel then(
          Array.set allFcArr !count {dim = (getCoord i frm); face = (getFcStr j); fc_id = ""};
          incr count) in
      if Array.length el.faces = 6 then(
        Array.iteri finder2 el.faces) in
    Array.iteri finder1 frm.blocks;
    allFcArr in

  (* join frameA and frameB at every possible combination in flA and flB *)
  let joinAB fLA fLB =
    let count = ref 0 in
    let joiner1 elA =
      let joiner2 elB =
        if elB.face = "Empty" then ignore() else(
          try(
            Array.set returnArr !count (join frameA (elA) frameB (elB));
            incr count)
          with
          | Face_Taken x          -> ignore()
          | Block_Overlap x       -> ignore()
          | Invalid_Face x        -> ignore()
          | Opposite_Face x       -> ignore()
          | Invalid_Block x       -> ignore()
          | Block_Out_Of_Bounds x -> ignore())in
      if elA.face = "Empty" then ignore() else(Array.iter joiner2 (fndFcsB elA fLB)) in
    Array.iter joiner1 fLA in

  (* Call join on specified faces of frameA and frameB *)
  let num fLA fLB = match (Array.length fLA, Array.length fLB) with
    | (0, 0) -> joinAB (allFc frameA) (allFc frameB)
    | (x, 0) -> joinAB fLA (allFc frameB)
    | (0, y) -> joinAB (allFc frameA) fLB
    | (x, y) -> joinAB fLA fLB in

  num faceArrA faceArrB;

  (* Remove duplicate and empty frames from results *)
  let returnList = Array.to_list returnArr in
  let returnList = List.sort_uniq compare returnList in
  let returnList = if (List.hd returnList).x = 0 then List.tl returnList else returnList in
  let returnArr = Array.of_list returnList in

  returnArr;;


  (* RETURN ONE FRAME VERSION OF BUILD *)
  (* build takes two frames and an array of face ID's for each frame, returns
   all possible frames made by joining the two original frames at the specified
   faces. If the faceID array is empty for either frame the algorithm assumes
   all open faces as possible join locations *)
let buildone frameA faceArrA frameB faceArrB returnstring =

  (* Create an array large enough to hold the maximum number of possible results *)
  let returnArr = match (Array.length faceArrA, Array.length faceArrB) with
      (0, 0)  ->  Array.init (6*(Array.length frameA.blocks)*(Array.length frameB.blocks)) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""})
    | (a, 0)  ->  Array.init (a*(Array.length frameA.blocks)) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""})
    | (0, b)  ->  Array.init (b*(Array.length frameB.blocks)) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""})
    | (a, b)  ->  Array.init (a*b) (fun _ -> {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}) in

  (* Return all faces in fLB that can be joined to face el *)
  let fndFcsB el fLB = match el.face with
    | "E" -> Array.map (fun x -> if (x.face = "W") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "W" -> Array.map (fun x -> if (x.face = "E") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "N" -> Array.map (fun x -> if (x.face = "S") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "S" -> Array.map (fun x -> if (x.face = "N") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "F" -> Array.map (fun x -> if (x.face = "B") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    | "B" -> Array.map (fun x -> if (x.face = "F") then x else {dim = (0,0,0); face = "Empty"; fc_id = ""}) fLB
    |  _  -> raise (Invalid_Face "A given face string for the first frame is not formated as one of: E, W, N, S, F, B") in

  (* Return all available faces in frm *)
  let allFc frm =
    let allFcArr = Array.init ((Array.length frm.blocks) * 6) (fun _ -> {dim = (0,0,0); face = "Empty"; fc_id = ""}) in
    let count = ref 0 in
    let finder1 i el =
      let finder2 j fel =
        if fel then(
          Array.set allFcArr !count {dim = (getCoord i frm); face = (getFcStr j); fc_id = ""};
          incr count) in
      if Array.length el.faces = 6 then(
        Array.iteri finder2 el.faces) in
    Array.iteri finder1 frm.blocks;
    allFcArr in

  (* join frameA and frameB at every possible combination in flA and flB *)
  let joinAB fLA fLB =
    let count = ref 0 in
    let joiner1 elA =
      let joiner2 elB =
        if elB.face = "Empty" then ignore() else(
          try(
            Array.set returnArr !count (join frameA (elA) frameB (elB));
            incr count)
          with
          | Face_Taken x          -> ignore()
          | Block_Overlap x       -> ignore()
          | Invalid_Face x        -> ignore()
          | Opposite_Face x       -> ignore()
          | Invalid_Block x       -> ignore()
          | Block_Out_Of_Bounds x -> ignore())in
      if elA.face = "Empty" then ignore() else(Array.iter joiner2 (fndFcsB elA fLB)) in
    Array.iter joiner1 fLA in

  (* Call join on specified faces of frameA and frameB *)
  let num fLA fLB = match (Array.length fLA, Array.length fLB) with
    | (0, 0) -> joinAB (allFc frameA) (allFc frameB)
    | (x, 0) -> joinAB fLA (allFc frameB)
    | (0, y) -> joinAB (allFc frameA) fLB
    | (x, y) -> joinAB fLA fLB in

  (* Returns the smallest overall frame from the build *)
  let smallest iA =
    let index = ref 0 in
    let xyz = ref 1000000 in
    let finder i el =
      if (el.x + el.y + el.z) < !xyz then(
        xyz := (el.x + el.y + el.z);
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns the largest overall frame from the build *)
  let largest iA =
    let index = ref 0 in
    let xyz = ref 0 in
    let finder i el =
      if (el.x + el.y + el.z) > !xyz then(
        xyz := (el.x + el.y + el.z);
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns the frame with the smallest x dimension from the build *)
  let smallestx iA =
    let index = ref 0 in
    let x = ref 1000000 in
    let finder i el =
      if el.x < !x then(
        x := el.x;
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns the frame with the largest x dimension from the build *)
  let largestx iA =
    let index = ref 0 in
    let x = ref 0 in
    let finder i el =
      if el.x > !x then(
        x := el.x;
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns the frame with the smallest y dimension from the build *)
   let smallesty iA =
    let index = ref 0 in
    let y = ref 1000000 in
    let finder i el =
      if el.y < !y then(
        y := el.y;
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns the frame with the largest y dimension from the build *)
   let largesty iA =
    let index = ref 0 in
    let y = ref 0 in
    let finder i el =
      if el.y > !y then(
        y := el.y;
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns the frame with the smallest z dimension from the build *)
  let smallestz iA =
    let index = ref 0 in
    let z = ref 1000000 in
    let finder i el =
      if el.z < !z then(
        z := el.z;
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns the frame with the largest z dimension from the build *)
  let largestz iA =
    let index = ref 0 in
    let z = ref 0 in
    let finder i el =
      if el.z > !z then(
        z := el.z;
        index := i) in
    Array.iteri finder iA;
    iA.(!index) in

  (* Returns a random frame from the build *)
  let random iA =
    let n = Random.int (Array.length iA) in
    Array.get iA n in

  (* Matches build return specifier, calls function to return *)
  let returner rs rA = match rs with
      "smallest"  ->  smallest rA
    | "largest"   ->  largest rA
    | "smallestx" ->  smallestx rA
    | "largestx"  ->  largestx rA
    | "smallesty" ->  smallesty rA
    | "largesty"  ->  largesty rA
    | "smallestz" ->  smallestz rA
    | "largestz"  ->  largestz rA
    | _           ->  random rA in

  num faceArrA faceArrB;

  (* Remove duplicate and empty frames from results *)
  let returnList = Array.to_list returnArr in
  let returnList = List.sort_uniq compare returnList in
  let returnList = if (List.hd returnList).x = 0 then List.tl returnList else returnList in
  let returnArr = Array.of_list returnList in

  if (Array.length returnArr) > 0 then returner returnstring returnArr else {x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""};;


(* Return a frame with the given dimensions *)
let frameCons x y z n =
  let arr = Array.init (x*y*z) (fun _ -> {faces = [|true;true;true;true;true;true|]}) in
  let frm = {x = x; y = y; z = z; blocks = arr; fr_id = n} in
  faceCheck frm.blocks x y z;
  frm;;

let execute (globals, functions) =
  print_endline "Add stack execution code here ...\n"
