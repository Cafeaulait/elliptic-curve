(** A field over [big_int] modulo [p]. *)

open Big_int_Z

let make_barrett_reduction p =
    let shift x t =
        (* assume 64 bit architecture. *)
        shift_right_big_int x (t * 64)
    in
    let mask x t =
        extract_big_int x 0 (t * 64)
    in
    let nth_power_of_b n =
        power_int_positive_int 2 (n * 64)
    in
    let t = num_digits_big_int p in
    let b2t = nth_power_of_b (2 * t) in
    let bt1 = nth_power_of_b (t + 1) in
    let m, _ = quomod_big_int b2t p in
    let barrett_reduction x =
	let q0 = shift x (t - 1) in
	let q = shift (mult_big_int m q0) (t + 1) in
	let r = sub_big_int (mask x (t + 1)) (mask (mult_big_int q p) (t + 1)) in
	let r = if sign_big_int r = -1 then add_big_int r bt1 else r in
	let rec continue r =
	    if lt_big_int r p then
		r
	    else 
		continue (sub_big_int r p)
	in
	continue r
    in
    barrett_reduction

(** Core operations in the field over [big_int] modulo [p]. *)
module MakeCore (O : Field.Order with type element = Z.t) : (Field.Core with type element = Z.t) = struct
  type element = Z.t

  open O

  let barrett_reduction = make_barrett_reduction p
  let zero = zero_big_int
  let one = unit_big_int
	      
  (* assume -p < x < 2p *)
  let adjust x =
      if sign_big_int x = -1 then
	  add_big_int x p
      else if lt_big_int x p then
	  x
      else
	  sub_big_int x p

  let of_int n = adjust (mod_big_int (big_int_of_int n) p)
  let to_int = int_of_big_int
  let to_bitlist = Z_big_int.to_bitlist
  let of_string s = adjust (mod_big_int (big_int_of_string s) p)
  let to_string = string_of_big_int
  let add x y = adjust (add_big_int x y)
  let unary_minus x = sub_big_int p x
  let sub x y = adjust (sub_big_int x y)
  let mul x y = barrett_reduction (mult_big_int x y)
  let square x = barrett_reduction (square_big_int x)
  let divmod = quomod_big_int
  let compare = compare_big_int
  let less = lt_big_int
  let less_equal = le_big_int
  let equal = eq_big_int
  let mod2 = Z_big_int.mod2
  let mod4 = Z_big_int.mod4 
  let mod8 = Z_big_int.mod8
  let nth_power_of_2 n = mod_big_int (Z_big_int.nth_power_of_2 n) p
  let shift_right = shift_right_big_int

  let shift_left x n =
      match n with
	  0 -> x
	| 1 -> barrett_reduction (shift_left_big_int x 1)
	| _ -> mod_big_int (shift_left_big_int x n) p

end

(*
module Make (O : Order) =
    struct
      module Core = MakeCore (O)

      module Op = Field.MakeGenericOperation (Varint2)

      include Core

      let log = Op.log
      let gcd = Op.gcd
      let extended_gcd = Op.extended_gcd

      let invert x =
	  Op.invert x p
	      
      let sqrt = Op.sqrt
	      
      let mass_add n add one zero minus_one =
	  let l = Naf.of_bit (Varint2.to_bit n) in
	  Op.mass_add l add one zero minus_one

      let mass_apply = Op.mass_apply

      let mass_apply2 n f v_one v_zero =
	  let cache = Array.create 16 v_zero in
	  let rec continue i v =
	      if i > 15 then
		  ()
	      else begin
		  cache.(i) <- v;
		  continue (i + 1) (f v_one v)
	      end
	  in
	  continue 0 v_zero;
	  let a = n.Varint.aval in
	  let rec continue i q =
	      if i < 0 then
		  q
	      else begin
		  let n = a.(i) in
		  let w = (n >> 20) land 15 in
		  let q =
		      if equal q v_zero then
			  q
		      else begin
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  q
		      end
		  in
		  let q = if w > 0 then f q cache.(w) else q in
		  let w = (n >> 16) land 15 in
		  let q =
		      if equal q v_zero then
			  q
		      else begin
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  q
		      end
		  in
		  let q = if w > 0 then f q cache.(w) else q in
		  let w = (n >> 12) land 15 in
		  let q =
		      if equal q v_zero then
			  q
		      else begin
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  q
		      end
		  in
		  let q = if w > 0 then f q cache.(w) else q in
		  let w = (n >> 8) land 15 in
		  let q =
		      if equal q v_zero then
			  q
		      else begin
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  q
		      end
		  in
		  let q = if w > 0 then f q cache.(w) else q in
		  let w = (n >> 4) land 15 in
		  let q =
		      if equal q v_zero then
			  q
		      else begin
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  q
		      end
		  in
		  let q = if w > 0 then f q cache.(w) else q in
		  let w = n land 15 in
		  let q =
		      if equal q v_zero then
			  q
		      else begin
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  let q = f q q in
			  q
		      end
		  in
		  let q = if w > 0 then f q cache.(w) else q in
		  continue (i - 1) q
	      end
	  in
	  if equal n zero then
	      v_zero
	  else if equal n one then
	      v_one
	  else begin
	      let length = Array.length a in
	      continue (length - 1) v_zero
	  end

      let power x n =
	  mass_apply2 n mul x one
	      
(*
      let legendre_symbol x =
	  Op.legendre_symbol x p
*)

      let legendre_symbol a =
	  let a =
	      if less a p && less_equal zero a then
		  a
	      else begin
		  let _, a = divmod a p in
		  a
	      end
	  in
	  let a = if less a zero then add a p else a in
	  if equal a zero then
	      0
	  else begin
	      let l = ref 1 in
	      let rec remove_power_of_four x =
		  if mod4 x = 0 then begin
			remove_power_of_four (shift_right x 2)
		    end
		    else
			x
	      in
	      let rec continue x y =
(*		  Printf.printf "Lx = %s\n" (Conv.string_of_varint2 x);
		  Printf.printf "Ly = %s\n" (Conv.string_of_varint2 y); *)
(*	    Printf.printf "l(%d/%d): %d\n" x y (!l); *)
		  let _, x = divmod x y in
(*		  Printf.printf "Lr = %s\n" (Conv.string_of_varint2 x); *)
		  let x =
		      if less (shift_right y 1) x then begin
			  if mod4 y = 3 then
			      l := -(!l);
			  sub y x
		      end
		      else
			  x
		  in
		  let x = remove_power_of_four x in
(*		  Printf.printf "L4 = %s\n" (Conv.string_of_varint2 x); *)
		  let x =
		      if mod2 x = 0 then begin
			  let y8 = mod8 y in
			  if y8 = 3 || y8 = 5 then
			      l := -(!l);
			  shift_right x 1
		      end
		      else
			  x
		  in
		  if equal x one then
		      !l
		  else begin
		      if mod4 x = 3 && mod4 y = 3 then
			  l := -(!l);
		      continue y x
		  end
	      in
	      continue a p
	  end

      let quadratic_nonresidue = ref None
	      
      let quadratic_residue a =
	  let n =
	      match !quadratic_nonresidue with
		  Some n -> n
		| None ->
		    let n = Op.find_quadratic_nonresidue p in
		    quadratic_nonresidue := Some n;
		    n
	  in
	  Op.quadratic_residue power mul n a p

      end
*)
