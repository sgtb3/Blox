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
		aface = frameA[ax][ay][az].open_faces[0]
	else if afacetr = "W" then 
		aface = frameA[ax][ay][az].open_faces[1]
	else if afacetr = "N" then 
		aface = frameA[ax][ay][az].open_faces[2]
	else if afacetr = "S" then 
		aface = frameA[ax][ay][az].open_faces[3]
	else if afacetr = "F" then 
		aface = frameA[ax][ay][az].open_faces[4]
		else if afacetr = "B" then 
	aface = frameA[ax][ay][az].open_faces[5] 
		else ignore();

	let bface = false in
	
	if bfacetr = "E" then(
		bface = frameB[bx][by][bz].open_faces[0];
		bx_shift = (ax - 1) - bx;
		by_shift = ay - by;
		bz_shift = az - bz)
	else if bfacetr = "W" then( 
		bface = frameB[bx][by][bz].open_faces[1];
		bx_shift = (ax + 1) - bx;
		by_shift = ay - by;
		bz_shift = az - bz)
	else if bfacetr = "N" then( 
		bface = frameB[bx][by][bz].open_faces[2];
		bx_shift = ax - bx;
		by_shift = (ay - 1) - by;
		bz_shift = az - bz)
	else if bfacetr = "S" then( 
		bface = frameB[bx][by][bz].open_faces[3];
		bx_shift = ax - bx;
		by_shift = (ay + 1) - by;
		bz_shift = az - bz) 
	else if bfacetr = "F" then( 
		bface = frameB[bx][by][bz].open_faces[4];
		bx_shift = ax - bx;
		by_shift = ay - by;
		bz_shift = (az - 1) - bz)
	else if bfacetr = "B" then( 
		bface = frameB[bx][by][bz].open_faces[5];
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
			((afacetr = "W") && not(bfacetr "W"))) then 
		prerr_string "Error: Illegal face option."
	else ignore();

	if (((afacetr = "N") && not(bfacetr = "S")) ||
		  ((afacetr = "S") && not(bfacetr "N"))) then 
		prerr_string "Error: Illegal face option."
	else ignore();
	
	if (((afacetr = "N") && not(bfacetr = "S")) ||
		  ((afacetr = "S") && not(bfacetr "N"))) then 
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
		
  (* create an entry for Frame "joins" list *)		
	
