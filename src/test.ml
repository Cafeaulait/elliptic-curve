
open Big_int_Z

module ZI : (Field.t with type element = int) = Z_int
module ZB : (Field.t with type element = Z.t) = Z_big_int

let test_hexadecimal() =
    let n = Z_big_int.big_int_of_hexadecimal "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012" in
    Printf.printf "%s\n" (string_of_big_int n);
    assert (string_of_big_int n = "602046282375688656758213480587526111916698976636884684818");
    let s = Z_big_int.hexadecimal_of_big_int n in
    Printf.printf "%s\n" s;
    assert (s = "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012");
    ()

let test_bitlist() =
    let to_string l =
        List.fold_left (^) "" (List.map string_of_int l)
    in
    let x = Random.int 10000000 in
    let xi = ZI.of_int x in
    let xb = ZB.of_int x in
    let zi = ZI.to_bitlist xi in
    let zb = ZB.to_bitlist xb in
    Printf.printf "%d = %s = %s\n" x (to_string zi) (to_string zb);
    assert (zi = zb);
    ()

let test_four_arithmetic_ops() =
    let x = 1 + Random.int 20000000 in
    let y = 1 + Random.int 20000000 in
    let xi, yi = ZI.of_int x, ZI.of_int y in
    let xb, yb = ZB.of_int x, ZB.of_int y in
    let zi = ZI.add xi yi in
    let zb = ZB.add xb yb in
    Printf.printf "%d + %d = %s = %s\n" x y (ZI.to_string zi) (ZB.to_string zb);
    flush stdout;
    assert (ZI.to_string zi = ZB.to_string zb);
    let wi = ZI.sub xi yi in
    let wb = ZB.sub xb yb in
    Printf.printf "%d - %d = %s = %s\n" x y (ZI.to_string wi) (ZB.to_string wb);
    flush stdout;
    assert (ZI.to_string wi = ZB.to_string wb);
    let ui = ZI.mul xi yi in
    let ub = ZB.mul xb yb in
    Printf.printf "%d * %d = %s = %s\n" x y (ZI.to_string ui) (ZB.to_string ub);
    flush stdout;
    assert (ZI.to_string ui = ZB.to_string ub);
    let qi, ri = ZI.divmod xi yi in
    let qb, rb = ZB.divmod xb yb in
    Printf.printf "%d / %d = (%s, %s) = (%s, %s)\n" x y
        (ZI.to_string qi) (ZI.to_string ri)
        (ZB.to_string qb) (ZB.to_string rb);
    flush stdout;
    assert (ZI.to_string qi = ZB.to_string qb);
    assert (ZI.to_string ri = ZB.to_string rb);
    ()

let test_square() =
    let x = Random.int 10000000 in
    let xi = ZI.of_int x in
    let xb = ZB.of_int x in
    let zi = ZI.square xi in
    let zb = ZB.square xb in
    Printf.printf "%d^2 = %s = %s\n" x (ZI.to_string zi) (ZB.to_string zb);
    assert (ZI.to_string zi = ZB.to_string zb);
    ()

let test_bitshift() =
    let x = Random.int 100000000 in
    let y = Random.int 28 in
    let xi = ZI.of_int x in
    let xb = ZB.of_int x in
    let zi = ZI.shift_right xi y in
    let zb = ZB.shift_right xb y in
    Printf.printf "%d >> %d = %s = %s\n" x y (ZI.to_string zi) (ZB.to_string zb);
    assert (ZI.to_string zi = ZB.to_string zb);
    let x = Random.int 1024 in
    let y = Random.int 18 in
    let xi = ZI.of_int x in
    let xb = ZB.of_int x in
    let zi = ZI.shift_left xi y in
    let zb = ZB.shift_left xb y in
    Printf.printf "%d << %d = %s = %s\n" x y (ZI.to_string zi) (ZB.to_string zb);
    assert (ZI.to_string zi = ZB.to_string zb);
    ()

let test_gcd() =
    let x = Random.int 10000 in
    let y = Random.int 10000 in
    let xi, yi = ZI.of_int x, ZI.of_int y in
    let xb, yb = ZB.of_int x, ZB.of_int y in
    let gi = ZI.gcd xi yi in
    let gb = ZB.gcd xb yb in
    Printf.printf "gcd(%d,%d) = %s = %s\n" x y (ZI.to_string gi) (ZB.to_string gb);
    flush stdout;
    assert (ZI.to_string gi = ZB.to_string gb);
    let ai, bi, ci = ZI.extended_gcd xi yi in
    let ab, bb, cb = ZB.extended_gcd xb yb in
    Printf.printf "%s x %s + %s x %s = %s\n" (ZI.to_string ai) (ZI.to_string xi)
        (ZI.to_string bi) (ZI.to_string yi) (ZI.to_string ci);
    flush stdout;
    assert (ZI.to_string ai = ZB.to_string ab);
    assert (ZI.to_string bi = ZB.to_string bb);
    assert (ZI.to_string ci = ZB.to_string cb);
    ()

let test_inversion p =
    let q = ZI.of_int p in
    for i = 1 to p - 1 do
        let a = ZI.of_int i in
	let x = Z_int.Op.invert a q in
	let w = ZI.mul a x in
	let _, b = ZI.divmod w q in
	Printf.printf "%s x %s = %s (mod %d)\n"
            (ZI.to_string a) (ZI.to_string x) (ZI.to_string b) p;
        flush stdout;
        assert (ZI.equal b ZI.one)
    done

let test_sqrt() =
    let x = Random.int 0x1ffffff in
    let xi = ZI.of_int x in
    let xb = ZB.of_int x in
    let ri = ZI.sqrt xi in
    let rb = ZB.sqrt xb in
    Printf.printf "sqrt(%d) = %s = %s\n" x (ZI.to_string ri) (ZB.to_string rb);
    assert (ZI.to_string ri = ZB.to_string rb);
    ()

let test_mass_add() =
    let n = Random.int 10000000 in
    let e = Random.int 1000 in
    let onei, minus_onei = ZI.of_int e, ZI.of_int (-e) in
    let oneb, minus_oneb = ZB.of_int e, ZB.of_int (-e) in
    let xi = ZI.mass_add (ZI.of_int n) ZI.add onei ZI.zero minus_onei in
    let xb = ZB.mass_add (ZB.of_int n) ZB.add oneb ZB.zero minus_oneb in
    Printf.printf "%s = %s\n" (ZI.to_string xi) (ZB.to_string xb);
    assert (ZI.to_string xi = ZB.to_string xb);
    assert (ZI.equal xi (ZI.of_int (n * e)));
    ()

let test_power() =
    let n = Random.int 10 in
    let e = Random.int 10 in
    let ni = ZI.of_int n in
    let nb = ZB.of_int n in
    let ei = ZI.of_int e in
    let eb = ZB.of_int e in
    let zi = ZI.power ei ni in
    let zb = ZB.power eb nb in
    Printf.printf "%d^%d = %s = %s\n" e n (ZI.to_string zi) (ZB.to_string zb);
    assert (ZI.to_string zi = ZB.to_string zb);
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
		match Z_big_int.Op.legendre_symbol (ZB.of_int a) (ZB.of_int p) with
		    -1 -> assert (number_of_sqrt.(a) = 0)
		  | 0 -> assert (a = 0)
		  | 1 -> assert (number_of_sqrt.(a) > 0)
		  | _ -> assert false
	    done;
	    Printf.printf " ok\n";
            flush stdout;
	end
    done

let test_barrett_reduction() =
    let p = shift_left_big_int (big_int_of_int (1 + Random.int 10000)) 64 in
    let barrett_reduction = F_big_int.make_barrett_reduction p in
    for i = 1 to 1000 do
	let x = mod_big_int (shift_left_big_int (big_int_of_int (Random.int 10000)) 64) p in
	let y = mod_big_int (shift_left_big_int (big_int_of_int (Random.int 10000)) 64) p in
	let z = mod_big_int (mult_big_int x y) p in
        let w = barrett_reduction (mult_big_int x y) in
	Printf.printf "%s * %s = %s = %s (mod %s)\n" (string_of_big_int x) (string_of_big_int y)
            (string_of_big_int z) (string_of_big_int w) (string_of_big_int p);
	assert (eq_big_int z w)
    done;
    flush stdout

let test_factorization() =
    for i = 1 to 1000 do 
	let n = 3 + Random.int (996 * 996) in
	Printf.printf "%d\n" n; flush stdout;
	let n = ZB.of_int n in
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
	let n = ZB.of_int (100001 + n * 2) in
	if Prime.is_prime_number_by_Pocklington_Lehmer n = is_prime n then
	    Printf.printf "%d: ok\n" (ZB.to_int n)
	else begin
	    Printf.printf "%d: ng\n" (ZB.to_int n);
	    assert false
	end;
	flush stdout
    done;
    ()

let test_Pocklington_Lehmer2() =
    let n = ZB.of_string "105554676553297" in
    Printf.printf "n = %s\n" (ZB.to_string n); flush stdout;
    assert (Prime.is_prime_number_by_Pocklington_Lehmer n);
    ()

module F1009 = F_big_int.Make (struct type element = Z.t let p = big_int_of_int 1009 end)

module ECSpec1009 = struct
  type element = Z.t

  let a = big_int_of_int 71
  let b = big_int_of_int 602
end

module E1009 = Ec.Make (F1009) (ECSpec1009)

let test_ec_ff() =
    let make_point x y =
	( (F1009.of_int x), (F1009.of_int y), F1009.one )
    in
    let get_point( x, y, z ) =
	let x, y = E1009.to_affine_coord( x, y, z ) in
        int_of_big_int x, int_of_big_int y
    in
(*    let plus p q =
	E1009.plus p q
    in *)
    let multiply n p =
	E1009.multiply (big_int_of_int n) p
    in
    E1009.check_parameter();
    let p = make_point 1 237 in
    let q = make_point 190 271 in
    let p10 = multiply 10 p in
    Printf.printf "[10]P = %s\n" (E1009.string_of_point p10);
    assert (get_point p10 = ( 32, 737 ));
    let q10 = multiply 10 q in
    Printf.printf "[10]Q = %s\n" (E1009.string_of_point q10);
    assert (get_point q10 = ( 592, 97 ));
    let p106 = multiply 106 p in
    Printf.printf "[106]P = %s\n" (E1009.string_of_point p106);
    assert (get_point p106 = ( 639, 160 ));
    let q106 = multiply 106 q in
    Printf.printf "[106]Q = %s\n" (E1009.string_of_point q106);
    assert (get_point q106 = ( 639, 849 ));
    let p265 = multiply 265 p in
    Printf.printf "[265]P = %s\n" (E1009.string_of_point p265);
    assert (get_point p265 = ( 50, 0 ));
    let q265 = multiply 265 q in
    Printf.printf "[265]Q = %s\n" (E1009.string_of_point q265);
    assert (get_point q265 = ( 50, 0 ));
    E1009.report();
    ()

module F22BIT = F_big_int.Make (struct type element = Z.t let p = big_int_of_int 4063417 end)

module ECSpec22BIT = struct
  type element = Z.t

  let a = big_int_of_int 83527
  let b = big_int_of_int 42987
end

module E22BIT = Ec.Make (F22BIT) (ECSpec22BIT)

let is_zero( _, _, z ) =
    sign_big_int z = 0

let test_ElGamal22() =
    let n = 4063417 in
    let make_point x y =
	( (F22BIT.of_int x), (F22BIT.of_int y), F22BIT.one )
    in
    let plus p q =
	E22BIT.plus p q
    in
    let multiply n p =
	E22BIT.multiply (big_int_of_int n) p
    in
    let equal_point p q =
	let x1, y1 = E22BIT.to_affine_coord p in
	let x2, y2 = E22BIT.to_affine_coord q in
	F22BIT.equal x1 x2 && F22BIT.equal y1 y2
    in
    let find_random_point() =
	let rec continue x =
	    let w = F22BIT.of_int x in
	    match E22BIT.find_y w with
		Ec.SINGLE y | Ec.DOUBLE( y, _ ) -> ( w, y, F22BIT.one )
	      |	Ec.NONE -> continue ((x + 1) mod n)
	in
	continue (Random.int n)
    in
    let g = make_point 0 2785936 in
    Printf.printf "F_%d\n" n;
    print_string "EC: y^2 = x^3 + 83527x + 42987\n";
    Printf.printf "g = %s\n" (E22BIT.string_of_point g);
    let og = multiply 4061081 g in
    Printf.printf "[4061081]g = %s\n" (E22BIT.string_of_point og);
    assert (is_zero og);
    for i = 0 to 1000 do
	let x = 3 + Random.int 100000 in
	let k = 1 + Random.int (n - 1) in
	let h = multiply x g in
	let m = find_random_point() in
	Printf.printf "secret key x = %d\n" x;
        Printf.printf "public key h = %s\n" (E22BIT.string_of_point h);
	Printf.printf "k = %d\n" k;
        Printf.printf "message m = %s\n" (E22BIT.string_of_point m);
	let a = multiply k g in
	let b = plus (multiply k h) m in
        Printf.printf "encrypted message = %s, %s\n"
            (E22BIT.string_of_point a) (E22BIT.string_of_point b);
	let c = E22BIT.unary_minus a in
	let c = multiply x c in
	let d = plus b c in
        Printf.printf "decoded message = %s\n" (E22BIT.string_of_point d);
	if equal_point m d then
	    print_string "ok\n"
	else
	    assert false
    done;
    ()

module P192BIT = struct
  type element = Z.t

  (* 2^192 - 2^64 - 1 *)
  let p = big_int_of_string "6277101735386680763835789423207666416083908700390324961279"
end

module F192BIT = F_big_int.Make (P192BIT)

module ECSpec192BIT = struct
  type element = Z.t

  let a = big_int_of_string "6277101735386680763835789423207666416083908700390324961276"
  let b = big_int_of_string "2455155546008943817740293915197451784769108058161191238065"

  let order = big_int_of_string "6277101735386680763835789423176059013767194773182842284081"
end

module E192BIT = Ec.Make (F192BIT) (ECSpec192BIT)

let test_ElGamal192() =
    let plus p q =
	E192BIT.plus p q
    in
    let multiply n p =
	E192BIT.multiply (big_int_of_int n) p
    in
    let equal_point p q =
	let x1, y1 = E192BIT.to_affine_coord p in
	let x2, y2 = E192BIT.to_affine_coord q in
	F192BIT.equal x1 x2 && F192BIT.equal y1 y2
    in
    let find_random_point() =
	let rec continue x =
	    let w = F192BIT.of_int x in
	    match E192BIT.find_y w with
		Ec.SINGLE y | Ec.DOUBLE( y, _ ) -> ( w, y, F192BIT.one )
	      |	Ec.NONE -> continue (x + 1)
	in
	continue (Random.int 100000)
    in
    let x = F192BIT.of_string "602046282375688656758213480587526111916698976636884684818" in
    let y = F192BIT.of_string "174050332293622031404857552280219410364023488927386650641" in
    let g = ( x, y, F192BIT.one ) in
    print_string "F_(2^192 - 2^64 - 1)\n";
    Printf.printf "p = %s\n" (F192BIT.to_string P192BIT.p);
    Printf.printf "g = %s\n" (E192BIT.string_of_point g);
    let og = E192BIT.multiply ECSpec192BIT.order g in
    Printf.printf "[-]g = %s\n" (E192BIT.string_of_point og);
    assert (is_zero og);
    for i = 0 to 1000 do
	let x = 3 + Random.int 100000 in
	let k = 1 + Random.int 100000 in
	let h = multiply x g in
	let m = find_random_point() in
	Printf.printf "secret key x = %d\n" x;
	Printf.printf "public key h = %s\n" (E192BIT.string_of_point h);
	Printf.printf "k = %d\n" k;
	Printf.printf "message m = %s\n" (E192BIT.string_of_point m);
	let a = multiply k g in
	let b = plus (multiply k h) m in
	Printf.printf "encrypted message = %s,%s\n" (E192BIT.string_of_point a)
	    (E192BIT.string_of_point b);
	let c = E192BIT.unary_minus a in
	let c = multiply x c in
	let d = plus b c in
	Printf.printf "decoded message = %s\n" (E192BIT.string_of_point d);
	if equal_point m d then
	    print_string "ok\n"
	else
	    assert false
    done;
    ()

let bench_ec192() =
    let x = F192BIT.of_string "602046282375688656758213480587526111916698976636884684818" in
    let y = F192BIT.of_string "174050332293622031404857552280219410364023488927386650641" in
    let g = ( x, y, F192BIT.one ) in
    print_string "192 bits F_(2^192 - 2^64 - 1)\n";
    Printf.printf "p = %s\n" (F192BIT.to_string P192BIT.p);
    Printf.printf "g = %s\n" (E192BIT.string_of_point g);
    let start_time = Unix.gettimeofday() in
    let og = E192BIT.multiply ECSpec192BIT.order g in
    let end_time = Unix.gettimeofday() in
    Printf.printf "[-]g = %s in %.3fms\n"
        (E192BIT.string_of_point og) ((end_time -. start_time) *. 1000.0);
    assert (is_zero og);
    ()

module P224BIT = struct
  type element = Z.t

  let p = big_int_of_string "26959946667150639794667015087019630673557916260026308143510066298881"
end

module F224BIT = F_big_int.Make (P224BIT)

module ECSpec224BIT = struct
  type element = Z.t

  let a = big_int_of_string "26959946667150639794667015087019630673557916260026308143510066298878"
  let b = F224BIT.of_hex "b4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4"

  let order = big_int_of_string "26959946667150639794667015087019625940457807714424391721682722368061"
end

module E224BIT = Ec.Make (F224BIT) (ECSpec224BIT)

let bench_ec224() =
    let x = F224BIT.of_hex "b70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21" in
    let y = F224BIT.of_hex "bd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34" in
    let g = ( x, y, F224BIT.one ) in
    print_string "224 bits\n";
    Printf.printf "p = %s\n" (F224BIT.to_string P224BIT.p);
    Printf.printf "g = %s\n" (E224BIT.string_of_point g);
    let start_time = Unix.gettimeofday() in
    let og = E224BIT.multiply ECSpec224BIT.order g in
    let end_time = Unix.gettimeofday() in
    Printf.printf "[-]g = %s in %.3fms\n"
        (E224BIT.string_of_point og) ((end_time -. start_time) *. 1000.0);
    assert (is_zero og);
    ()

module P521BIT = struct
  type element = Z.t

  let p = big_int_of_string "6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151"
end

module F521BIT = F_big_int.Make (P521BIT)

module ECSpec521BIT = struct
  type element = Z.t

  let a = big_int_of_string "6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057148"
  let b = F521BIT.of_hex "051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00"

  let order = big_int_of_string "6864797660130609714981900799081393217269435300143305409394463459185543183397655394245057746333217197532963996371363321113864768612440380340372808892707005449"
end

module E521BIT = Ec.Make (F521BIT) (ECSpec521BIT)

let bench_ec521() =
    let x = F521BIT.of_hex "c6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66" in
    let y = F521BIT.of_hex "11839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650" in
    let g = ( x, y, F521BIT.one ) in
    print_string "521 bits\n";
    Printf.printf "p = %s\n" (F521BIT.to_string P521BIT.p);
    Printf.printf "g = %s\n" (E521BIT.string_of_point g);
    let start_time = Unix.gettimeofday() in
    let og = E521BIT.multiply ECSpec521BIT.order g in
    let end_time = Unix.gettimeofday() in
    Printf.printf "[-]g = %s in %.3fms\n"
        (E521BIT.string_of_point og) ((end_time -. start_time) *. 1000.0);
    assert (is_zero og);
    ()

let main() =
    let seed = int_of_float (Unix.time()) in
    Random.init seed;
    Printf.printf "seed = %d\n" seed;
    test_hexadecimal();
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
    test_barrett_reduction();
    test_factorization();
    test_Pocklington_Lehmer();
    test_Pocklington_Lehmer2();
    test_ec_ff();
    test_ElGamal22();
    test_ElGamal192();
    bench_ec192();
    bench_ec224();
    bench_ec521();
    ()

;;
if not (!Sys.interactive) then
    main();;
