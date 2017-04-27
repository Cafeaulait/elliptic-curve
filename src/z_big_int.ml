(** A ring of [big_int]. *)

open Big_int_Z

let hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
             '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |]

let hexadecimal_of_big_int n =
    (* assume 64 bit architecture. *)
    let length = 64 * (num_digits_big_int n) in
    let unit = 32 in
    let s = Bytes.create (length / 4) in
    for i = 0 to length / unit - 1 do
        let x = int_of_big_int (extract_big_int n (i * unit) unit) in
        for j = 0 to (unit / 4) - 1 do
            Bytes.set s ((length / unit - 1 - i) * (unit / 4) + (unit / 4 - 1 - j)) hex.((x lsr (j * 4)) land 0xf)
        done
    done;
    let start =
        let i = ref 0 in
        while !i < length / 4 && s.[!i] = '0' do
            incr i
        done;
        !i
    in
    if start > 0 then
        Bytes.sub s start (length / 4 - start)
    else
        s

let big_int_of_hexadecimal s =
    let length = String.length s in
    let get_digit i =
        let c = s.[i] in
        if c >= '0' && c <= '9' then
            (int_of_char c) - (int_of_char '0')
        else if c >= 'a' && c <= 'f' then
            (int_of_char c) - (int_of_char 'a') + 10
        else if c >= 'a' && c <= 'f' then
            (int_of_char c) - (int_of_char 'a') + 10
        else 
            failwith "invalid char in hex string"
    in
    let rec continue i n j z =
        if i + j = length then
            add_int_big_int n (shift_left_big_int z (i * 4))
        else if i = 12 then 
            continue 0 0 (i + j) (add_int_big_int n (shift_left_big_int z (i * 4)))
        else
            continue (i + 1) ((n lsl 4) lor (get_digit (i + j))) j z
    in
    continue 0 0 0 zero_big_int

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
  let of_hex = big_int_of_hexadecimal
  let to_hex = hexadecimal_of_big_int
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
