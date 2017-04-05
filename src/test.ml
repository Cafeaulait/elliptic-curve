
module FI = F_int
module FB = F_big_int

let test_bitlist() =
    let to_string l =
        List.fold_left (^) "" (List.map string_of_int l)
    in
    let x = Random.int 10000000 in
    let xi = FI.of_int x in
    let xb = FB.of_int x in
    let zi = FI.to_bitlist xi in
    let zb = FB.to_bitlist xb in
    Printf.printf "%d = %s = %s\n" x (to_string zi) (to_string zb);
    assert (zi = zb);
    ()

let test_four_arithmetic_ops() =
    let x = 1 + Random.int 20000000 in
    let y = 1 + Random.int 20000000 in
    let xi, yi = FI.of_int x, FI.of_int y in
    let xb, yb = FB.of_int x, FB.of_int y in
    let zi = FI.add xi yi in
    let zb = FB.add xb yb in
    Printf.printf "%d + %d = %s = %s\n" x y (FI.to_string zi) (FB.to_string zb);
    flush stdout;
    assert (FI.to_string zi = FB.to_string zb);
    let wi = FI.sub xi yi in
    let wb = FB.sub xb yb in
    Printf.printf "%d - %d = %s = %s\n" x y (FI.to_string wi) (FB.to_string wb);
    flush stdout;
    assert (FI.to_string wi = FB.to_string wb);
    let ui = FI.mul xi yi in
    let ub = FB.mul xb yb in
    Printf.printf "%d * %d = %s = %s\n" x y (FI.to_string ui) (FB.to_string ub);
    flush stdout;
    assert (FI.to_string ui = FB.to_string ub);
    let qi, ri = FI.divmod xi yi in
    let qb, rb = FB.divmod xb yb in
    Printf.printf "%d / %d = (%s, %s) = (%s, %s)\n" x y
        (FI.to_string qi) (FI.to_string ri)
        (FB.to_string qb) (FB.to_string rb);
    flush stdout;
    assert (FI.to_string qi = FB.to_string qb);
    assert (FI.to_string ri = FB.to_string rb);
    ()

let test_square() =
    let x = Random.int 10000000 in
    let xi = FI.of_int x in
    let xb = FB.of_int x in
    let zi = FI.square xi in
    let zb = FB.square xb in
    Printf.printf "%d^2 = %s = %s\n" x (FI.to_string zi) (FB.to_string zb);
    assert (FI.to_string zi = FB.to_string zb);
    ()

let test_bitshift() =
    let x = Random.int 100000000 in
    let y = Random.int 28 in
    let xi = FI.of_int x in
    let xb = FB.of_int x in
    let zi = FI.shift_right xi y in
    let zb = FB.shift_right xb y in
    Printf.printf "%d >> %d = %s = %s\n" x y (FI.to_string zi) (FB.to_string zb);
    assert (FI.to_string zi = FB.to_string zb);
    let x = Random.int 1024 in
    let y = Random.int 18 in
    let xi = FI.of_int x in
    let xb = FB.of_int x in
    let zi = FI.shift_left xi y in
    let zb = FB.shift_left xb y in
    Printf.printf "%d << %d = %s = %s\n" x y (FI.to_string zi) (FB.to_string zb);
    assert (FI.to_string zi = FB.to_string zb);
    ()

let test_gcd() =
    let x = Random.int 10000 in
    let y = Random.int 10000 in
    let xi, yi = FI.of_int x, FI.of_int y in
    let xb, yb = FB.of_int x, FB.of_int y in
    let gi = FI.gcd xi yi in
    let gb = FB.gcd xb yb in
    Printf.printf "gcd(%d,%d) = %s = %s\n" x y (FI.to_string gi) (FB.to_string gb);
    flush stdout;
    assert (FI.to_string gi = FB.to_string gb);
    let ai, bi, ci = FI.extended_gcd xi yi in
    let ab, bb, cb = FB.extended_gcd xb yb in
    Printf.printf "%s x %s + %s x %s = %s\n" (FI.to_string ai) (FI.to_string xi)
        (FI.to_string bi) (FI.to_string yi) (FI.to_string ci);
    flush stdout;
    assert (FI.to_string ai = FB.to_string ab);
    assert (FI.to_string bi = FB.to_string bb);
    assert (FI.to_string ci = FB.to_string cb);
    ()

let test_inversion p =
    let q = FI.of_int p in
    for i = 1 to p - 1 do
        let a = FI.of_int i in
	let x = FI.Op.invert a q in
	let w = FI.mul a x in
	let _, b = FI.divmod w q in
	Printf.printf "%s x %s = %s (mod %d)\n"
            (FI.to_string a) (FI.to_string x) (FI.to_string b) p;
        flush stdout;
        assert (FI.equal b FI.one)
    done

let test_sqrt() =
    let x = Random.int 0x1ffffff in
    let xi = FI.of_int x in
    let xb = FB.of_int x in
    let ri = FI.sqrt xi in
    let rb = FB.sqrt xb in
    Printf.printf "sqrt(%d) = %s = %s\n" x (FI.to_string ri) (FB.to_string rb);
    assert (FI.to_string ri = FB.to_string rb);
    ()

let test_mass_add() =
    let n = Random.int 10000000 in
    let e = Random.int 1000 in
    let onei, minus_onei = FI.of_int e, FI.of_int (-e) in
    let oneb, minus_oneb = FB.of_int e, FB.of_int (-e) in
    let xi = FI.mass_add (FI.of_int n) FI.add onei FI.zero minus_onei in
    let xb = FB.mass_add (FB.of_int n) FB.add oneb FB.zero minus_oneb in
    Printf.printf "%s = %s\n" (FI.to_string xi) (FB.to_string xb);
    assert (FI.to_string xi = FB.to_string xb);
    assert (FI.equal xi (FI.of_int (n * e)));
    ()

let test_power() =
    let n = Random.int 10 in
    let e = Random.int 10 in
    let ni = FI.of_int n in
    let nb = FB.of_int n in
    let ei = FI.of_int e in
    let eb = FB.of_int e in
    let zi = FI.power ei ni in
    let zb = FB.power eb nb in
    Printf.printf "%d^%d = %s = %s\n" e n (FI.to_string zi) (FB.to_string zb);
    assert (FI.to_string zi = FB.to_string zb);
    ()

let test_legendre_symbol() =
    for p = 3 to 999 do
	if Prime.is_prime_under_1000 p then begin
	    Printf.printf "checking (a/%d)" p;
            flush stdout;
	    let number_of_sqrt = Array.make p 0 in
	    for a = 1 to p - 1 do
		let n = (a * a) mod p in
		number_of_sqrt.(n) <- number_of_sqrt.(n) + 1
	    done;
	    for a = 0 to p - 1 do
		match FB.Op.legendre_symbol (FB.of_int a) (FB.of_int p) with
		    -1 -> assert (number_of_sqrt.(a) = 0)
		  | 0 -> assert (a = 0)
		  | 1 -> assert (number_of_sqrt.(a) > 0)
		  | _ -> assert false
	    done;
	    Printf.printf " ok\n";
            flush stdout;
	end
    done

(*
open Arith

module PrimintOp = Field.MakeGenericOperation (Primint)

module Varint2Op = Field.MakeGenericOperation (Varint2)

module Varint10Op = Field.MakeGenericOperation (Varint10)

module Varint2Op2 = Varint.MakeOperation (Varint2)

let test_barrett_reduction() =
    let barrett_reduction = Varint2Op2.make_barrett_reduction (Varint2.of_int 10000) in
    for i = 1 to 100 do
	let x = Random.int 10000 in
	let y = Random.int 10000 in
	let z = (x * y) mod 10000 in
	let xr = Varint2.of_int x in
	let yr = Varint2.of_int y in
	let zr = barrett_reduction (Varint2.mul xr yr) in
	Printf.printf "%d * %d = %d = %d (mod 10000)\n" x y (x * y) (Varint2.to_int zr);
	assert (z = Varint2.to_int zr)
    done

let test_factorization() =
    for i = 1 to 1000 do 
	let n = 3 + Random.int (996 * 996) in
	Printf.printf "%d\n" n; flush stdout;
	let n = Varint2.of_int n in
	let l1, a = Prime.factorize_by_Pollard_Miller_Rabin n in
	print_string " = ";
	Prime.print_factor_list l1;
	print_newline();
	flush stdout;
	assert (a = Prime.TOTALLY_DECOMPOSED);
	let l2 = Prime.factorize_by_all_search n in
	print_string " = ";
	Prime.print_factor_list l2;
	print_newline();
	flush stdout;
	assert (l1 = l2);
	()
    done;
    ()

let test_Pocklington_Lehmer() =
    let is_prime n =
	match Prime.factorize_by_all_search n with
	    [ _, 1 ] -> true
	  | _ -> false
    in
    for n = 0 to 1000 do
	let n = Varint2.of_int (100001 + n * 2) in
	if Prime.ascertain_prime_number_by_Pocklington_Lehmer n = is_prime n then
	    Printf.printf "%d: ok\n" (Varint2.to_int n)
	else begin
	    Printf.printf "%d: ng\n" (Varint2.to_int n);
	    assert false
	end;
	flush stdout
    done;
    ()

let bench_inversion() =
    let start_time = Sys.time() in
    for p = 3 to 999 do
	if Prime.is_prime_under_1000 p then begin
	    for a = 1 to p - 1 do
		let _ = PrimintOp.invert a p in
		()
	    done
	end
    done;
    let end_time = Sys.time() in
    Printf.printf "inversion (prim): %.2fs\n" (end_time -. start_time);
    let start_time = Sys.time() in
    for p = 3 to 999 do
	if Prime.is_prime_under_1000 p then begin
	    let pr = Varint10.of_int p in
	    for a = 1 to p - 1 do
		let _ = Varint10Op.invert (Varint10.of_int a) pr in
		()
	    done
	end
    done;
    let end_time = Sys.time() in
    Printf.printf "inversion (varint10): %.2fs\n" (end_time -. start_time);
    let start_time = Sys.time() in
    for p = 3 to 999 do
	if Prime.is_prime_under_1000 p then begin
	    let pr = Varint2.of_int p in
	    for a = 1 to p - 1 do
		let _ = Varint2Op.invert (Varint2.of_int a) pr in
		()
	    done
	end
    done;
    let end_time = Sys.time() in
    Printf.printf "inversion (varint2): %.2fs\n" (end_time -. start_time);
    ()

let bench_sqrt() =
    let start_time = Sys.time() in
    for n = 1 to 20000 do
	let _ = PrimintOp.sqrt n in
	()
    done;
    let end_time = Sys.time() in
    Printf.printf "sqrt (prim): %.2fs\n" (end_time -. start_time);
    let start_time = Sys.time() in
    for n = 1 to 20000 do
	let _ = Varint10Op.sqrt (Varint10.of_int n) in
	()
    done;
    let end_time = Sys.time() in
    Printf.printf "sqrt (varint10): %.2fs\n" (end_time -. start_time);
    let start_time = Sys.time() in
    for n = 1 to 20000 do
	let _ = Varint2Op.sqrt (Varint2.of_int n) in
	()
    done;
    let end_time = Sys.time() in
    Printf.printf "sqrt (varint2): %.2fs\n" (end_time -. start_time);
    ()

let bench_legendre_symbol() =
    let start_time = Sys.time() in
    for p = 3 to 999 do
	if Prime.is_prime_under_1000 p then begin
	    for a = 0 to p - 1 do
		let _ = PrimintOp.legendre_symbol a p in
		()
	    done
	end
    done;
    let end_time = Sys.time() in
    Printf.printf "legendre symbol (prim): %.2fs\n" (end_time -. start_time);
    let start_time = Sys.time() in
    for p = 3 to 999 do
	if Prime.is_prime_under_1000 p then begin
	    let pr = Varint10.of_int p in
	    for a = 0 to p - 1 do
		let _ = Varint10Op.legendre_symbol (Varint10.of_int a) pr in
		()
	    done
	end
    done;
    let end_time = Sys.time() in
    Printf.printf "legendre symbol (varint10): %.2fs\n" (end_time -. start_time);
    let start_time = Sys.time() in
    for p = 3 to 999 do
	if Prime.is_prime_under_1000 p then begin
	    let pr = Varint2.of_int p in
	    for a = 0 to p - 1 do
		let _ = Varint2Op.legendre_symbol (Varint2.of_int a) pr in
		()
	    done
	end
    done;
    let end_time = Sys.time() in
    Printf.printf "legendre symbol (varint2): %.2fs\n" (end_time -. start_time);
    ()

let bench_factorization() =
    let start_time = Sys.time() in
    for n = 996 * 996 - 10000 to 996 * 996 do
(*	Printf.printf "n = %d\n" n; flush stdout; *)
	let n = Varint2.of_int n in
	let _ = Prime.factorize_by_all_search n in
	()
    done;
    let end_time = Sys.time() in
    Printf.printf "fact (all search): %.2fs\n" (end_time -. start_time); flush stdout;
    let start_time = Sys.time() in
    for n = 996 * 996 - 10000 to 996 * 996 do
(*	Printf.printf "n = %d\n" n; flush stdout; *)
	let n = Varint2.of_int n in
	let _ = Prime.factorize_by_Pollard_Miller_Rabin n in
	()
    done;
    let end_time = Sys.time() in
    Printf.printf "fact (Pollard): %.2fs\n" (end_time -. start_time); flush stdout;
    ()

(*
module FP997 = Primintmod.Make (struct let p = 997 end)

module FB997 = Varint2mod.Make (struct let p = "997" end)

let bench_quadratic_residue() =
    let p = 997 in
    let start_time = Sys.time() in
    for n = 1 to p - 1 do
	match FP997.legendre_symbol n with
	    1 ->
	      let _ = FP997.quadratic_residue n in
	      ()
	  | _ -> ()
    done;
    let end_time = Sys.time() in
    Printf.printf "quadratic residue (prim): %.2fs\n" (end_time -. start_time);
    let start_time = Sys.time() in
    for n = 1 to p - 1 do
	let n = Varint2.of_int n in
	match FB997.legendre_symbol n with
	    1 ->
	      let _ = FB997.quadratic_residue n in
	      ()
	  | _ -> ()
    done;
    let end_time = Sys.time() in
    Printf.printf "quadratic residue (varint2): %.2fs\n" (end_time -. start_time);
    ()

module F1009 = Varint2mod.Make (struct let p = "1009" end)

module ECSpec1009 =
    struct
      type element = Varint2.element

      let a = Varint2.of_int 71
      let b = Varint2.of_int 602
    end

module E1009 = Ec.Make (F1009) (ECSpec1009)

let print_point ( x, y, z ) =
    if Varint2.equal z Varint2.zero then
	print_string "o"
    else begin
	let x, y = E1009.to_affine_coord( x, y, z ) in
	print_string "(";
	print_string (Conv.string_of_varint2 x);
	print_string ",";
	print_string (Conv.string_of_varint2 y);
	print_string ")"
    end

let test_ec_ff() =
    let make_point x y =
	( (Varint2.of_int x), (Varint2.of_int y), Varint2.one )
    in
    let plus p q =
	E1009.plus p q
    in
    let multiply n p =
	E1009.multiply (Varint2.of_int n) p
    in
    let p = make_point 1 237 in
    let q = make_point 190 271 in
    print_string "[10]P = "; print_point (multiply 10 p); print_newline();
    print_string "[10]Q = "; print_point (multiply 10 q); print_newline();
    print_string "[106]P = "; print_point (multiply 106 p); print_newline();
    print_string "[106]Q = "; print_point (multiply 106 q); print_newline();
    print_string "[265]P = "; print_point (multiply 265 p); print_newline();
    print_string "[265]Q = "; print_point (multiply 265 q); print_newline();
    let p = make_point 32 737 in
    let q = make_point 592 97 in
    print_string "[2]P + [0]Q = "; print_point (plus (multiply 2 p) (multiply 0 q));
    print_newline();
    print_string "[1]P + [1]Q = "; print_point (plus (multiply 1 p) (multiply 1 q));
    print_newline();
    print_string "[3]P + [4]Q = "; print_point (plus (multiply 3 p) (multiply 4 q));
    print_newline();
    ()

module F22BIT = Varint2mod.Make (struct let p = "4063417" end)

module ECSpec22BIT =
    struct
      type element = Varint2.element

      let a = Varint2.of_int 83527
      let b = Varint2.of_int 42987
    end

module E22BIT = Ec.Make (F22BIT) (ECSpec22BIT)

let print_point ( x, y, z ) =
    if Varint2.equal z Varint2.zero then
	print_string "o"
    else begin
	let x, y = E22BIT.to_affine_coord( x, y, z ) in
	print_string "(";
	print_string (Conv.string_of_varint2 x);
	print_string ",";
	print_string (Conv.string_of_varint2 y);
	print_string ")"
    end

let test_ElGamal22() =
    let n = 4063417 in
    let make_point x y =
	( (Varint2.of_int x), (Varint2.of_int y), Varint2.one )
    in
    let plus p q =
	E22BIT.plus p q
    in
    let multiply n p =
	E22BIT.multiply (Varint2.of_int n) p
    in
    let equal_point p q =
	let x1, y1 = E22BIT.to_affine_coord p in
	let x2, y2 = E22BIT.to_affine_coord q in
	Varint2.equal x1 x2 && Varint2.equal y1 y2
    in
    let find_random_point() =
	let rec continue x =
	    let w = Varint2.of_int x in
	    match E22BIT.find_y w with
		Ec.SINGLE y | Ec.DOUBLE( y, _ ) -> ( w, y, Varint2.one )
	      |	Ec.NONE -> continue ((x + 1) mod n)
	in
	continue (Random.int n)
    in
    let g = make_point 0 2785936 in
    Printf.printf "F_%d\n" n;
    print_string "E: y^2 = x^3 + 83527x + 42987\n";
    print_string "g = "; print_point g; print_newline();
    print_string "[4061081]g = "; print_point (multiply 4061081 g); print_newline();
    for i = 0 to 1000 do
	let x = 3 + Random.int 100000 in
	let k = 1 + Random.int (n - 1) in
	let h = multiply x g in
	let m = find_random_point() in
	Printf.printf "secret key x = %d\n" x;
	print_string "public key h = "; print_point h; print_newline();
	Printf.printf "k = %d\n" k;
	print_string "message m = "; print_point m; print_newline();
	let a = multiply k g in
	let b = plus (multiply k h) m in
	print_string "encrypted message = "; print_point a; print_string ", "; print_point b; print_newline();
	let c = E22BIT.unary_minus a in
	let c = multiply x c in
	let d = plus b c in
	print_string "decoded message = "; print_point d; print_newline();
	if equal_point m d then
	    print_string "ok\n"
	else
	    assert false
    done;
    ()

*)

module P192BIT = 
    struct
      (* 2^192 - 2^64 - 1 *)
      let p = "6277101735386680763835789423207666416083908700390324961279"
	      
    end

module F192BIT = Varint2mod.Make (P192BIT)

module ECSpec192BIT =
    struct
      type element = Varint2.element

      let a =
	  Conv.varint2_of_string "6277101735386680763835789423207666416083908700390324961276"

      let b =
	  Conv.varint2_of_string "2455155546008943817740293915197451784769108058161191238065"

      let order =
	  Conv.varint2_of_string "6277101735386680763835789423176059013767194773182842284081"

    end

module E192BIT = Ec.Make (F192BIT) (ECSpec192BIT)

let string_of_affine_point ( x, y, z ) =
    if Varint2.equal z Varint2.zero then
	"o"
    else begin
	let x, y = E192BIT.to_affine_coord( x, y, z ) in
	"("^(Conv.string_of_varint2 x)^","^(Conv.string_of_varint2 y)^")"
    end

let string_of_projective_point ( x, y, z ) =
    "("^(Conv.string_of_varint2 x)^","^(Conv.string_of_varint2 y)^","^(Conv.string_of_varint2 z)^")"

let test_ElGamal192() =
    let plus p q =
	E192BIT.plus p q
    in
    let multiply n p =
	E192BIT.multiply (Varint2.of_int n) p
    in
    let equal_point p q =
	let x1, y1 = E192BIT.to_affine_coord p in
	let x2, y2 = E192BIT.to_affine_coord q in
	Varint2.equal x1 x2 && Varint2.equal y1 y2
    in
    let find_random_point() =
	let rec continue x =
	    let w = Varint2.of_int x in
	    match E192BIT.find_y w with
		Ec.SINGLE y | Ec.DOUBLE( y, _ ) -> ( w, y, Varint2.one )
	      |	Ec.NONE -> continue (x + 1)
	in
	continue (Random.int 100000)
    in
    let x =
	Conv.varint2_of_string "602046282375688656758213480587526111916698976636884684818"
    in
    let y =
	Conv.varint2_of_string "174050332293622031404857552280219410364023488927386650641"
    in
    let g = ( x, y, Varint2.one ) in
    print_string "F_(2^192 - 2^64 - 1)\n";
    Printf.printf "p = %s\n" (Conv.string_of_varint2 F192BIT.p);
    Printf.printf "g = %s\n" (string_of_projective_point g);
(*
    let g2 = plus g g in
    Printf.printf "g + g = %s\n" (string_of_projective_point g2);
    if true then
	exit 0;
*)
    let order =
	Conv.varint2_of_string "6277101735386680763835789423176059013767194773182842284081"
    in
    Printf.printf "[-]g = %s\n" (string_of_affine_point (E192BIT.multiply order g));
    for i = 0 to 10 do
	let x = 3 + Random.int 100000 in
	let k = 1 + Random.int 100000 in
	let h = multiply x g in
	let m = find_random_point() in
	Printf.printf "secret key x = %d\n" x;
	Printf.printf "public key h = %s\n" (string_of_affine_point h);
	Printf.printf "k = %d\n" k;
	Printf.printf "message m = %s\n" (string_of_affine_point m);
	let a = multiply k g in
	let b = plus (multiply k h) m in
	Printf.printf "encrypted message = %s,%s\n" (string_of_affine_point a)
	    (string_of_affine_point b);
	let c = E192BIT.unary_minus a in
	let c = multiply x c in
	let d = plus b c in
	Printf.printf "decoded message = %s\n" (string_of_affine_point d);
	if equal_point m d then
	    print_string "ok\n"
	else
	    assert false
    done;
    ()

(*


module E1 = Ec.Make (Varint2)

let test_ec_plus() =
    let make_point( x, y ) =
	(Varint2.of_int x), (Varint2.of_int y), Varint2.one
    in
    let print_point( x, y, z ) =
	let x = Varint2.to_int x in
	let y = Varint2.to_int y in
	let z = Varint2.to_int z in
	print_string "(";
	print_int (x / z);
	print_string ",";
	print_int (y / z);
	print_string ")"
    in
    let test a b l =
	let p = Array.of_list (List.map make_point l) in
	let length = Array.length p in
	for i = 0 to length - 1 do
	    for j = 0 to length - 1 do
		print_point p.(i);
		print_string " + ";
		print_point p.(j);
		print_string " = ";
		let p3 = E1.plus a b p.(i) p.(j) in
		if p3 = E1.o then
		    print_string "o"
		else
		    print_point p3;
		print_newline()
	    done
	done
    in
    (* y^2 = x^3 + 1. Z/6Z *)
    let a = Varint2.zero in
    let b = Varint2.one in
    E1.check_parameter a b;
    let l = [(0,1); (0,-1); (2,3); (2,-3); (-1,0)] in
    print_string "y^2 = x^3 + 1:\n";
    test a b l;
    (* y^2 = x^3 + 4x. Z/4Z *)
    let a = Varint2.of_int 4 in
    let b = Varint2.zero in
    E1.check_parameter a b;
    let l = [(0,0); (2,4); (2,-4)] in
    print_string "\ny^2 = x^3 + 4x:\n";
    test a b l;
    ()
    
let test_ec192() =
    let vi2_of_string s =
	Conv.decimal_to_binary32 (Varint10.of_string s)
    in
    let p = vi2_of_string "6277101735386680763835789423207666416083908700390324961279" in
    let a = vi2_of_string "6277101735386680763835789423207666416083908700390324961276" in
    let b = vi2_of_string "2455155546008943817740293915197451784769108058161191238065" in
    let x = vi2_of_string "602046282375688656758213480587526111916698976636884684818" in
    let y = vi2_of_string "174050332293622031404857552280219410364023488927386650641" in
    let order = vi2_of_string "6277101735386680763835789423176059013767194773182842284081" in
    let e = Ec.make_ec (Varint2mod.make) p a b in
    let g = ( x, y, Varint2.one ) in
    let print_point p = print_point e p in
    let v = e.Ec.multiply order g in
    print_string "[order]g = ";
    print_point v;
    print_newline();
    let blank_area = ref 0 in
    let max_blank_area = ref 0 in
    for x = 100000 to 100000 + 40000 do
	let x = Varint2.of_int x in
	let v = e.Ec.value x in
	if Varint2Op.legendre_symbol v p >= 0 then begin
	    blank_area := 0
	end
	else begin
	    incr blank_area;
	    if !blank_area > !max_blank_area then
		max_blank_area := !blank_area
	end
    done;
    Printf.printf "max blank area size = %d\n" (!max_blank_area);
    ()
*)

let main() =
    let seed = int_of_float (Unix.time()) in
    Random.init seed;
    Printf.printf "seed = %d\n" seed;

(*
    let x =
	Conv.varint2_of_string "348100664587244062809715104560438820728046977854773301282"
    in
(*
    let y =
	Conv.varint2_of_string "174050332293622031404857552280219410364023488927386650641"
    in
*)
    let x = { x with Varint.aval = Array.sub x.Varint.aval 0 2 } in
(*
    let y = { y with Varint.aval = Array.sub y.Varint.aval 3 2 } in
*)
    let z = Varint2.square x in
    Printf.printf "z = %s\n" (Conv.string_of_varint2 z);
    let x10 = Conv.binary_to_decimal x in
(*    let y10 = Conv.binary_to_decimal y in *)
    let z10 = Varint10.square x10 in
    let z2 = Conv.decimal_to_binary z10 in
    if Varint2.equal z z2 then
	print_string "z is correct\n"
    else
	print_string "z is wrong\n";

    Printf.printf "x = %s\n" (Varint2.to_string x);
(*    Printf.printf "y = %s\n" (Varint2.to_string y); *)
    Printf.printf "z = %s\n" (Varint2.to_string z);
    Printf.printf "z = %s (correct)\n" (Varint2.to_string z2);
    if true then
	exit 1;
*)

(*
    test_barrett_reduction();
    test_legendre_symbol();
    test_factorization();
    test_Pocklington_Lehmer();
    for i = 1 to 100 do
	test_square()
    done;
    bench_inversion();
    bench_sqrt();
    bench_legendre_symbol();
    bench_factorization();
    bench_quadratic_residue();
*)
(*
    test_ec_ff();
    test_ec_plus();
    test_ec_order();
    test_ec192();
    test_ElGamal22();
*)
    test_ElGamal192();
    ()
*)

let main() =
    let seed = int_of_float (Unix.time()) in
    Random.init seed;
    Printf.printf "seed = %d\n" seed;
    for _ = 1 to 1000 do
        test_bitlist();
        test_four_arithmetic_ops();
        test_square();
        test_bitshift();
        test_gcd();
        test_sqrt();
        test_mass_add();
        test_power()
    done;
    test_inversion 997;
    test_legendre_symbol();
    ()

;;
if not (!Sys.interactive) then
    main();;
