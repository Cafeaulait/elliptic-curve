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

module Make (O : Field.Order with type element = Z.t) : (Field.t with type element = Z.t) = struct
  open O

  module Core = MakeCore (O)
  module Op = Field.MakeGenericOperation (Core)

  include Core

  let log = Z_big_int.log
  let gcd = Z_big_int.gcd
  let extended_gcd = Z_big_int.extended_gcd

  let invert x =
      Z_big_int.Op.invert x p

  let sqrt = Z_big_int.sqrt

  let mass_add = Z_big_int.mass_add
  let mass_apply = Z_big_int.mass_apply
  let power = Op.power
  let legendre_symbol x = Z_big_int.Op.legendre_symbol x p

  let quadratic_nonresidue = ref None

  (** The Legendre symbol. *)
  let quadratic_residue a =
      let n =
	  match !quadratic_nonresidue with
	      Some n -> n
	    | None ->
		let n = Op.find_quadratic_nonresidue p in
		quadratic_nonresidue := Some n;
		n
      in
      Op.quadratic_residue n a p
end
