(** A ring of [int]. *)

(** Core operations in the ring of [int]. *)
module Core : (Field.Core with type element = int) = struct
  type element = int

  let zero = 0
  let one = 1
  let of_int x = x
  let to_int x = x

  let find_first_non_zero_bit n =
      assert (n >= 0);
      let rec continue bit =
	  if bit = 0 then
	      0
	  else if n land bit = 0 then
	      continue (bit lsr 1)
	  else
	      bit
      in
      (* assume 64 bit architecture. *)
      continue 0x2000000000000000

  let to_bitlist n =
      let rec continue bit =
	  if bit = 0 then
	      []
	  else
	      (if n land bit = 0 then 0 else 1)::continue (bit lsr 1)
      in
      continue (find_first_non_zero_bit n)

  let of_string = int_of_string
  let to_string = string_of_int
  let of_hex s = int_of_string ("0x"^s)
  let to_hex n = Printf.sprintf "%x" n
  let add x y = x + y
  let unary_minus x = -x
  let sub x y = x - y
  let mul x y = x * y
  let square x = x * x
  let divmod x y = x / y, x mod y
  let compare (x:int) y = compare x y
  let less x y = x < y
  let less_equal x y = x <= y
  let equal x y = x = y
  let mod2 x = x land 1
  let mod4 x = x land 3
  let mod8 x = x land 7
  let nth_power_of_2 n = 1 lsl n
  let shift_right x n = x asr n
  let shift_left x n = x lsl n
end

include Core

module Op = Field.MakeGenericOperation (Core)

let log = Op.log
let gcd = Op.gcd
let extended_gcd = Op.extended_gcd

let invert x =
    if x = 1 then
	1
    else if x = -1 then
	-1
    else
	raise (Failure "invert")

let sqrt = Op.sqrt
let mass_add = Op.mass_add
let mass_apply = Op.mass_apply
let power = Op.power

let legendre_symbol n =
    assert (n >= 0);
    if n = 0 then
	0
    else if square (sqrt n) = n then
	1
    else
	-1

let quadratic_residue = Op.sqrt
