(** A field over [int] modulo [p]. *)

(** Core operations in the field over [int] modulo [p]. *)
module MakeCore (O : Field.Order with type element = int) : (Field.Core with type element = int) = struct
  type element = int

  open O

  let zero = 0
  let one = 1

  (* assume -p < x < 2p *)
  let adjust x =
      if x < 0 then
	  x + p
      else if x < p then
	  x
      else
	  x - p

  let of_int n = adjust (n mod p)
  let to_int n = n
  let to_bitlist = Z_int.to_bitlist
  let of_string s = adjust ((int_of_string s) mod p)
  let to_string = string_of_int
  let add x y = adjust (x + y)
  let unary_minus x = p - x
  let sub x y = adjust (x - y)
  let mul x y = (x * y) mod p
  let square x = (x * x) mod p
  let divmod x y = x / y, x mod y
  let compare (x:int) y = compare x y
  let less x y = x < y
  let less_equal x y = x <= y
  let equal x y = x = y
  let mod2 x = x land 1
  let mod4 x = x land 3
  let mod8 x = x land 7
  let nth_power_of_2 n = (1 lsl n) mod p
  let shift_right x n = x asr n
  let shift_left x n = (x lsl n) mod p
end

module Make (O : Field.Order with type element = int) : (Field.t with type element = int) = struct
  open O

  module Core = MakeCore (O)
  module Op = Field.MakeGenericOperation (Core)

  include Core

  let log = Z_int.log
  let gcd = Z_int.gcd
  let extended_gcd = Z_int.extended_gcd

  let invert x =
      Z_int.Op.invert x p

  let sqrt = Z_int.sqrt
  let mass_add = Z_int.mass_add
  let mass_apply = Z_int.mass_apply
  let power = Op.power
  let legendre_symbol a = Z_int.Op.legendre_symbol a p

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
      Op.quadratic_residue n a p
end
