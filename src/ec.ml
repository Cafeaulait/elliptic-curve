(** Elliptic curves. *)

(** Given {i x}, this indicates solutions of {i y}. *)
type 't solution =
    NONE | SINGLE of 't | DOUBLE of 't * 't

(** This module indicates an elliptic curve {i y^2 = x^3 + ax + b}. *)
module type Spec =
    sig
      type element
      val a : element
      val b : element
    end

(** This functor defines elliptic curve S on field F, and basic operations on it. *)
module Make (F : Field.t) (S : Spec with type element = F.element) =
    struct
      open F
      open S

      let n4 = of_int 4
      let n27 = of_int 27

      (** Unit element. We use projective coordinates to represent a point. *)
      let o = ( zero, one, zero )
	  
      (** Check if the equations has distinct roots. *)
      let check_parameter() =
	  let d = add (mul n4 (mul a (square a))) (mul n27 (square b)) in
	  assert (not (equal d zero));
	  ()

      (** Transform a projective coordinate to an affine coordinate. *)
      let to_affine_coord( x, y, z ) =
	  (* assume ( x, y, z ) != o *)
	  if equal z one then
	      ( x, y )
	  else begin
	      let z = invert z in
	      ( (mul x z), (mul y z) )
	  end

      (** Transform an affine coordinate to a projective coordinate. *)
      let to_projective_coord( x, y ) =
	  ( x, y, one )

      (** Convert a point to a string. *)
      let string_of_point( x, y, z ) =
          if equal z zero then
	      "o"
          else begin
	      let x, y = to_affine_coord( x, y, z ) in
	      "("^(to_string x)^","^(to_string y)^")"
          end

      (** Inversion. *)
      let unary_minus( x, y, z ) =
	  if equal z zero then
	      o
	  else begin
	      let z = invert z in
	      let x = mul x z in
	      let y = mul y z in
	      let y = unary_minus y in
	      ( x, y, one )
	  end

      let counter1 = ref 0
      let counter2 = ref 0
      let counter3 = ref 0

      let report() =
	  Printf.printf "ec: the numbers of addition operations P + Q, (P = Q) %d, (P = -Q) %d, (P != Q) %d\n"
              (!counter1) (!counter2) (!counter3);
	  ()

      (** Addition. *)
      let plus ( x1, y1, z1 ) ( x2, y2, z2 ) =
	  if equal z1 zero then
	      ( x2, y2, z2 )
	  else if equal z2 zero then
	      ( x1, y1, z1 )
	  else begin
	      let x1z2 = mul x1 z2 in
	      let x2z1 = mul x2 z1 in
	      let u = sub x1z2 x2z1 in
	      let v = sub (mul y1 z2) (mul y2 z1) in
	      if equal u zero then begin
		  if equal v zero then begin
		      incr counter1;
		      (* P = Q *)
		      let x = x1 in
		      let y = y1 in
		      let z = z1 in
		      let x2 = square x in
		      let y2 = square y in
		      let z2 = square z in
		      let xy = mul x y in
		      let _2xy = shift_left xy 1 in
		      let _4xy = shift_left _2xy 1 in
		      let _2x2 = shift_left x2 1 in
		      let _2y2 = shift_left y2 1 in
		      let yz = mul y z in
		      let r = add (add _2x2 x2) (mul a z2) in
		      let t = shift_left yz 1 in
		      let s = sub (square r) (mul _4xy t) in
		      let t2 = square t in
		      if equal t zero then
			  o
		      else
			  ( (mul s t), (sub (mul (sub (mul _2xy t) s) r) (mul _2y2 t2)), (mul t2 t) )
		  end
		  else begin
		    (* P = -Q *)
		      incr counter2;
		      assert (equal (add (mul y1 z2) (mul y2 z1)) zero);
		      o
		  end
	      end
	      else begin
		  incr counter3;
		  (* P != Q *)
		  let u2 = square u in
		  let v2 = square v in
		  let z1z2 = mul z1 z2 in
		  let z1z2u2 = mul z1z2 u2 in
		  let w = sub (mul z1z2 v2) (mul (add x1z2 x2z1) u2) in
		  let y3 = sub (F.unary_minus (mul v w)) (mul z1z2u2 (sub (mul x1 y2) (mul x2 y1))) in
		  ( (mul u w), y3, (mul z1z2u2 u) )
	      end
	  end
      
      (** Multiplication. *)
      let multiply n p =
	  let q = unary_minus p in
	  mass_add n plus p o q

      (** Compute {i x^3 + ax + b}. *)
      let value x =
	  let x2 = square x in
	  add (mul x x2) (add (mul a x) b)

      (** Given {i x}, find the solutions {i y} of {i y^2 = x^3 + ax + b}. *)
      let find_y x =
	  let v = value x in
	  match legendre_symbol v with
	      1 ->
		let y1 = quadratic_residue v in
		let y2 = F.unary_minus y1 in
		if mod2 y1 = 0 then
		    DOUBLE( y1, y2 )
		else
		    DOUBLE( y2, y1 )
	    | 0 -> SINGLE (quadratic_residue v)
	    | _ -> NONE
    end
