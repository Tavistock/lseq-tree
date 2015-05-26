let boundary := 10; ;; Any constant
let S := {}; ;; map<depth,boolean>
;; true: boundary+
;; false: boundary-

function alloc(p, q)
	let depth := 0;
	let interval := 0;
	while (interval < 1) do ;; Not enough for 1 insert
		depth ++;
		interval := pref ix(q, depth) - prefix(p, depth) - 1;
	end while
	let step := min(boundary, interval); ;; Process the maximum step to stay between p and q

	if not(S.exist(depth)) then ;; add the new entry
		let rand := RandBool();
		S.set(depth, rand);
	end if
	if S.get(depth) then ;; boundary+
		let addV al := RandInt(0, step) + 1;
		let id := pref ix(p, depth) + addV al;
	else ;; boundary-
		let subV al := RandInt(0, step) + 1;
		let id := pref ix(q, depth) - subV al;
	end if
	return id;
end function

function prefix(id, depth)
	let idCopy := [];
	for (cpt := 1 to depth) do
		if (cpt < id.size) then ;; Copy the value
			idCopy = idCopy.append(id.at(cpt));
		else ;; Add 0 encoded in the right base
			idCopy = idCopy.append(0base(cpt));
		end if
	end for
	return idCopy;
end function
