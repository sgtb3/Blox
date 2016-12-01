

(* faceCheck function *)

(* Helper function for Join*)
let frm_mod xarg yarg zarg blcksarg =
  let f = {x = xarg; y = yarg; z = zarg; blocks = blcksarg} in f


(*JOIN FXN*)
(*UNCHECKED AND UNTESTED*)
let join frameA (a,b,c,d) frameB (e,f,g,h) = 
	let ax = a in 
	let ay = b in
	let az = c in
	let afacetr = d in
	
	let bx = e in
	let by = f in
	let bz = g in
	let bfacetr = h in
	
	let ax_shift = 0 in
	let ay_shift = 0 in
	let az_shift = 0 in
	
	let bx_shift = 0 in
	let by_shift = 0 in
	let bz_shift = 0 in 

	if frameA = frameB then prerr_string "Error: Attempting to join blocks from the same Frame."
	else ignore();
			
	let aface = false in 
	
	if afacetr = "E" then 
		aface = Array.get frameA[ax][ay][az].faces 0
	else if afacetr = "W" then 
		aface = Array.get frameA[ax][ay][az].faces 1
	else if afacetr = "N" then 
		aface = Array.get frameA[ax][ay][az].faces 2
	else if afacetr = "S" then 
		aface = Array.get frameA[ax][ay][az].faces 3
	else if afacetr = "F" then 
		aface = Array.get frameA[ax][ay][az].faces 4
	else if afacetr = "B" then 
		aface = Array.get frameA[ax][ay][az].faces 5 
	else ignore();

	let bface = false in
	
	if bfacetr = "E" then(
		bface = Array.get frameB[bx][by][bz].faces 0;
		bx_shift = (ax - 1) - bx;
		by_shift = ay - by;
		bz_shift = az - bz)
	else if bfacetr = "W" then( 
		bface = Array.get frameB[bx][by][bz].faces 1;
		bx_shift = (ax + 1) - bx;
		by_shift = ay - by;
		bz_shift = az - bz)
	else if bfacetr = "N" then( 
		bface = Array.get frameB[bx][by][bz].faces 2;
		bx_shift = ax - bx;
		by_shift = (ay - 1) - by;
		bz_shift = az - bz)
	else if bfacetr = "S" then( 
		bface = Array.get frameB[bx][by][bz].faces 3;
		bx_shift = ax - bx;
		by_shift = (ay + 1) - by;
		bz_shift = az - bz) 
	else if bfacetr = "F" then( 
		bface = Array.get frameB[bx][by][bz].faces 4;
		bx_shift = ax - bx;
		by_shift = ay - by;
		bz_shift = (az - 1) - bz)
	else if bfacetr = "B" then( 
		bface = Array.get frameB[bx][by][bz].faces 5;
		bx_shift = ax - bx;
		by_shift = ay - by;
		bz_shift = (az + 1) - bz)
	else ignore();
	
	(* check if frameA's block face is available *)
	if not(aface) then 
		prerr_string; "Error: Block face is not available for Join with"
	else ignore();
	
	(* check if frameB's block face is available *)
	if not(bface) then 
		prerr_string; "Error: Block face is not available for Join with"
	else ignore();
	
	(* check for opposite faces *)
	if (((afacetr = "E") && not(bfacetr = "W")) ||
			((afacetr = "W") && not(bfacetr "E"))) then 
		prerr_string "Error: Illegal face option."
	else ignore();

	if (((afacetr = "N") && not(bfacetr = "S")) ||
		  ((afacetr = "S") && not(bfacetr "N"))) then 
		prerr_string "Error: Illegal face option."
	else ignore();
	
	if (((afacetr = "F") && not(bfacetr = "B")) ||
		  ((afacetr = "B") && not(bfacetr "F"))) then 
		prerr_string "Error: Illegal face option."
	else ignore();

	(*  ========== ALL CHECKS PASSED. BEGIN JOIN PROCESS ========== *)
		
  (* create an entry for Frame "joins" list *)		
	type joins_entry = {frameone:frame; coordinatesone:int*int*int*int; frametwo:frame; coordinatestwo:int*int*int*int} in
	frameA.joins.add = join_entry in  (* add the join entry into A's joins *)
  frameB.joins.add = join_entry in  (* add the join entry into B's joins *)
	
	(* Determine shift values for A and B *)
	if bx_shift < 0 then(
		ax_shift = -bx_shift;
		bx_shift = 0)
	else ignore();
	
	if by_shift < 0 then(
		ay_shift = -by_shift;
		by_shift = 0)
	else ignore();
		
	if bz_shift < 0 then(
		az_shift = -bz_shift;
		bz_shift = 0)
	else ignore();
		
	(* Determine size of new array *)
	let cx_max = frameA.x + ax_shift in
	if cx_max < (frameB.x + bx_shift) then 
		cx_max = frameB.x + bx_shift
	else ignore();
		
	let cy_max = frameA.y + ay_shift in
	if cy_max < (frameB.y + by_shift) then 
		cy_max = frameB.y + by_shift
	else ignore();

	let cz_max = frameA.z + az_shift in
	if cz_max < (frameB.z + bz_shift) then 
		cz_max = frameB.z + bz_shift
	else ignore();


	(* Create new array of blocks *)
	let Cblcks = Array.init cx_max (fun _ -> Array.init cy_max (fun _ -> (Array.init cz_max 
		(fun _ -> let b = {faces = [|false;false;false;false;false;false|] } in b )))) in

	(* Fill Cblcks with blocks from array A *)
	let x = ref 0 in

	for i = 0 to frameA.x - 1 do
		let y = ref 0 in

		for j = 0 to frameA.y - 1 do
			let z = ref 0 in

			for k = 0 to frameA.z - 1 do
				let b = (Array.get (Array.get (Array.get frameA.blocks x) y) z) in
				if Array.length b.faces = 6 then (Array.set (Array.get (Array.get Cblcks x) y) z b)
				else ignore();

				incr z
			done;
			incr y
		done;
		incr x
	done;


	(* Fill Cblcks with blocks from array B *)
	let x = ref 0 in

	for i = 0 to frameB.x - 1 do
		let y = ref 0 in

		for j = 0 to frameB.y - 1 do
			let z = ref 0 in

			for k = 0 to frameB.z - 1 do
				let b = (Array.get (Array.get (Array.get frameB.blocks x) y) z) in
				if Array.length b.faces = 6 then (Array.set (Array.get (Array.get Cblcks x) y) z b)
				else ignore();

				incr z
			done;
			incr y
		done;
		incr x
	done;

    (* Run faceCheck !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
	(*faceCheck Cblcks*)

	(* Update Frame A with Cblocks array to finish merge of B into A *)
	let A = frm_mod cx_max cy_max cz_max Cblcks



