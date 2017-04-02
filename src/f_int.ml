(** A field over int. *)

(** The core operations in a field over int. *)
module Core : (Field.Core with type element = int) = struct
  type element = int

  let zero = 0
  let one = 1
  let of_int x = x
  let to_int x = x
  let of_string = int_of_string
  let to_string = string_of_int
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

(*

let sqrt = Op.sqrt

let mass_add n add one zero minus_one =
    let l = Naf.of_int n in
    Op.mass_add l add one zero minus_one

let mass_apply = Op.mass_apply

let power x n =
    mass_apply n mul x 1

let legendre_symbol n =
    assert (n >= 0);
    if n = 0 then
	0
    else begin
	let r = sqrt n in
	if square r = n then
	    1
	else
	    -1
    end

let quadratic_residue = Op.sqrt
*)
