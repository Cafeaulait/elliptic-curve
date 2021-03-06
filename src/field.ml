(** Generic definitions for finite fields. *)

(** Overflow or underflow during an arithmetic operation. *)
exception Overflow

(** The order of a field. {i p} denotes the order of the field. *)
module type Order = sig
  type element

  val p : element
end

(** Core operations in a field. *)
module type Core = sig
  type element

  (** Zero. *)
  val zero : element

  (** The unit element. *)
  val one : element

  (** Conversions from/to int. *)
  val of_int : int -> element
  val to_int : element -> int

  (** returns a list of 0 or 1. *)
  val to_bitlist : element -> int list

  (** Conversions from/to string. *)
  val of_string : string -> element
  val to_string : element -> string

  (** Conversions from/to hexadecimal. *)
  val of_hex : string -> element
  val to_hex : element -> string
  val add : element -> element -> element
  val unary_minus : element -> element
  val sub : element -> element -> element
  val mul : element -> element -> element
  val square : element -> element
  val divmod : element -> element -> element * element
  val compare : element -> element -> int
  val less : element -> element -> bool
  val less_equal : element -> element -> bool
  val equal : element -> element -> bool
  val mod2 : element -> int
  val mod4 : element -> int
  val mod8 : element -> int
  val nth_power_of_2 : int -> element
  val shift_right : element -> int -> element
  val shift_left : element -> int -> element
end

(** The type of a field. *)
module type t = sig
  include Core

  val log : element -> element -> int
  val gcd : element -> element -> element

  (** [extended_gcd x y] returns a triple {i ( a, b, c )} s.t. {i ax + by = c = gcd(x,y)}, provided that both x and y >= 0. *)
  val extended_gcd : element -> element -> element * element * element

  (** Finds the inverse element. *)
  val invert : element -> element

  (** Computes [sqrt n] in the Babylonian method. {i n} must be positive. *)
  val sqrt : element -> element

  (** [mass_add n add one zero minus_one] computes the summation of {i n} elements of [one]. *)
  val mass_add : element -> ('t -> 't -> 't) -> 't -> 't -> 't -> 't

  (** Let * be a commutative and associative binary operator.
      [mass_apply n f u e] computes {i u * u * ... * u} or {i u^n}
      provided that [f] is a prefix notation of * i.e. [f x y] = {i x * y}.
      [e] is the unit of * i.e. {i x * e = x}. *)
  val mass_apply : element -> ('t -> 't -> 't) -> 't -> 't -> 't

  (** [power x n] computes [x^n]. *)
  val power : element -> element -> element

  (** The Legendre symbol. *)
  val legendre_symbol : element -> int

  (** Quadratic residue. *)
  val quadratic_residue : element -> element
end
  
(** Generic operations derived from the core operations. *)
module MakeGenericOperation (C : Core) = struct
  open C

  (** Finds the maximum integer {i x} s.t. {i b^x <= n} provided that {i b, n >= 0}. *)
  let log b n =
      let approximate c =
          let rec continue x b =
	      let b2 = square b in
	      if less n (mul c b2) then
		  x, mul c b
	      else
		  continue (x + x) b2
	  in
	  if less n (mul c b) then
	      0, c
	  else
	      continue 1 b
      in
      let rec continue c =
	  let x, c = approximate c in
	  if x = 0 then
	      0
	  else
	      x + continue c
      in
      if less_equal b one then
	  if equal b n then
	      1
	  else
	      raise Overflow
      else
	  continue one

  (** compute gcd(x,y) assuming both x and y >= 0. *)
  let rec gcd x y =
      if less x y then
	  gcd y x
      else if equal y zero then
	  x
      else begin
	  let _, r = divmod x y in
	  if equal r zero then
	      y
	  else
	      gcd y r
      end

  (** [extended_gcd x y] returns a triple {i ( a, b, c )} s.t. {i ax + by = c = gcd(x,y)}, provided that both x and y >= 0. *)
  let extended_gcd x y =
      let rec continue a0 b0 c0 a1 b1 c1 =
	  (* invariant: ax + by = c *)
	  if equal c1 zero then begin
	      a0, b0, c0
	  end
	  else begin
	      let q, c2 = divmod c0 c1 in
	      let a2 = sub a0 (mul q a1) in
	      let b2 = sub b0 (mul q b1) in
	      continue a1 b1 c1 a2 b2 c2
	  end
      in
      if less x y then
	  continue zero one y one zero x
      else
	  continue one zero x zero one y

  (** [invert a p] returns {i a}{^ -1} mod {i p} provided that {i p} is a prime number and {i 0} < {i a} < {i p}. *)
  let invert a p =
      let x, _, g = extended_gcd a p in
      if not (equal g one) then begin
	  Printf.printf "computing invert %s (mod %s)\n" (to_string a) (to_string p);
	  assert false
      end;
      let _, x = divmod x p in
      let x = if less x zero then add x p else x in
      x

  (** Computes [sqrt n] in the Babylonian method. {i n} must be positive. *)
  let sqrt n =
      assert (less_equal zero n);
      let rec continue r =
	  if less_equal (square r) n && less n (square (add r one)) then
	      r
	  else begin
	      let s, _ = divmod n r in
	      let r = shift_right (add s r) 1 in
	      continue r
	  end
      in
      continue one

  (** [mass_add n add one zero minus_one] computes the summation of {i n} elements of [one]. *)
  let mass_add n add one zero minus_one =
      let naf_list = Naf.of_bitlist (to_bitlist n) in
      let rec continue = function
	  [] -> zero
	| [0] -> zero
	| [1] -> one
	| [-1] -> minus_one
	| b::l ->
	    let q = continue l in
	    if b = 0 then
		add q q
	    else if b = 1 then
		add q (add q one)
	    else
		add q (add q minus_one)
      in
      continue (List.rev naf_list)

  (** Let * be a commutative and associative binary operator.
      [mass_apply n f u e] computes {i u * u * ... * u} or {i u^n}
      provided that [f] is a prefix notation of * i.e. [f x y] = {i x * y}.
      [e] is the unit of * i.e. {i x * e = x}. *)
  (* e: the unit of multiplication
     u: the unit of addition. *)
  let mass_apply n f u e =
      let rec continue n =
	  if equal n zero then
	      e
	  else if equal n one then
	      u
	  else if mod2 n = 0 then begin
	      (* even number *)
	      let q = continue (shift_right n 1) in
	      f q q
	  end
	  else begin
	      (* odd number *)
	      let q = continue (shift_right n 1) in
	      f q (f q u)
	  end
      in
      continue n

  (** [power x n] computes [x^n]. *)
  let power x n =
    assert (less_equal zero n);
    mass_apply n mul x one

  (** The Legendre symbol. *)
  let legendre_symbol a p =
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
	      if mod4 x = 0 then
		  remove_power_of_four (shift_right x 2)
	      else
		  x
	  in
	  let rec continue x y =
	      let _, x = divmod x y in
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

  (** Find a quadratic non-residue. *)
  let find_quadratic_nonresidue p =
      let find_nonresidue() =
	  let rec continue x =
	      assert (less x p);
	      if legendre_symbol x p = -1 then
		  x
	      else
		  continue (add x one)
	  in
	  continue (of_int 2)
      in
      find_nonresidue()

  (** [quadratic_residue n a p] finds a quadratic residue of [a] mod [p].
      [n] can be an arbitrary quadratic non-residue. *)
  let quadratic_residue quadratic_nonresidue a p =
      let a_inv = invert a p in
      let pm1 = sub p one in
      let n = quadratic_nonresidue in
      let s, e =
          let rec continue s e = 
              if mod2 s = 0 then
                  continue (shift_right s 1) (e + 1)
              else
                  s, e
          in
          continue pm1 0
      in
      let b0 = power n s in
      let r = power a (shift_right (add s one) 1) in
      let rec continue j b =
          let t = mul b r in
          if j = e - 1 then
              t
          else begin
              let d = mul a_inv (square t) in
              let f = power d (nth_power_of_2 (e - 1 - j - 1)) in
              if equal f one then
                  continue (j + 1) b
              else if equal f pm1 then
                  let b_k_1 = power b0 (nth_power_of_2 j) in
                  continue (j + 1) (mul b b_k_1)
              else
                  assert false
          end
      in
      continue 0 one
end
