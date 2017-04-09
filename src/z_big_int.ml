(** A ring of [big_int]. *)

open Big_int_Z

(** Core operations in the ring over [big_int]. *)
module Core : (Field.Core with type element = Z.t) = struct
  type element = Z.t

  let zero = zero_big_int
  let one = unit_big_int
  let of_int = big_int_of_int
  let to_int = int_of_big_int

  let to_bitlist n =
      let rec continue bitmask =
          if lt_big_int n bitmask then
              []
          else if eq_big_int zero (and_big_int n bitmask) then
              0::continue (shift_left_big_int bitmask 1)
          else
              1::continue (shift_left_big_int bitmask 1)
      in
      List.rev (continue one)

  let of_string = big_int_of_string
  let to_string = string_of_big_int
  let add = add_big_int
  let unary_minus = minus_big_int
  let sub = sub_big_int
  let mul = mult_big_int
  let square = square_big_int
  let divmod = quomod_big_int
  let compare = compare_big_int
  let less = lt_big_int
  let less_equal = le_big_int
  let equal = eq_big_int
  let mod2 x = (int_of_big_int (extract_big_int x 0 8)) land 1
  let mod4 x = (int_of_big_int (extract_big_int x 0 8)) land 3
  let mod8 x = (int_of_big_int (extract_big_int x 0 8)) land 7
  let nth_power_of_2 n = power_int_positive_int 2 n
  let shift_right = shift_right_big_int
  let shift_left = shift_left_big_int
end

(*
  these functions might be useful?

val abs_big_int : Z.t -> Z.t
val succ_big_int : Z.t -> Z.t
val add_int_big_int : int -> Z.t -> Z.t
val pred_big_int : Z.t -> Z.t
val mult_int_big_int : int -> Z.t -> Z.t
val max_big_int : Z.t -> Z.t -> Z.t
val min_big_int : Z.t -> Z.t -> Z.t
val div_big_int : Z.t -> Z.t -> Z.t
val mod_big_int : Z.t -> Z.t -> Z.t
val gcd_big_int : Z.t -> Z.t -> Z.t
val power : Z.t -> int -> Z.t
val power_big : Z.t -> Z.t -> Z.t
val power_int_positive_int : int -> int -> Z.t
val power_big_int_positive_int : Z.t -> int -> Z.t
val power_int_positive_big_int : int -> Z.t -> Z.t
val power_big_int_positive_big_int : Z.t -> Z.t -> Z.t
val sign_big_int : Z.t -> int
val is_int_big_int : Z.t -> bool
val and_big_int : Z.t -> Z.t -> Z.t
val or_big_int : Z.t -> Z.t -> Z.t
val xor_big_int : Z.t -> Z.t -> Z.t
*)

include Core

module Op = Field.MakeGenericOperation (Core)

let log = Op.log
let gcd = Op.gcd
let extended_gcd = Op.extended_gcd

let invert x =
    if equal (abs_big_int x) one then
	x
    else
	raise (Failure "invert")

let sqrt = sqrt_big_int
let mass_add = Op.mass_add
let mass_apply = Op.mass_apply
let power = Op.power

let legendre_symbol n =
    assert (less_equal zero n);
    if equal n zero then
	0
    else if equal (square (sqrt n)) n then
	1
    else
	-1

let quadratic_residue = Op.sqrt
