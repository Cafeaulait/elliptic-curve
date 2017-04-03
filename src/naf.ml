(** Non adjacent form representation. *)

let of_bitlist l =
    let rec continue cj = function
	[] -> []
      |	kj::[] ->
	  let kj1 = 0 in
	  let cj1 = (kj + kj1 + cj) / 2 in
	  if cj1 = 0 then
	      (kj + cj - 2 * cj1)::[]
	  else
	      (kj + cj - 2 * cj1)::cj1::[]
      |	kj::(kj1::_ as l) ->
	  let cj1 = (kj + kj1 + cj) / 2 in
	  (kj + cj - 2 * cj1)::continue cj1 l
    in
    List.rev (continue 0 (List.rev l))

let to_int l =
    let rec continue n = function
	[] -> n
      | b::l -> continue (n + n + b) l
    in
    continue 0 l
