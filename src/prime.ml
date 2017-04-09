(** Operations on prime numbers. *)

open Z_big_int

let n2 = add one one
let n3 = of_int 3
let n5 = of_int 5
let n7 = of_int 7
let n1000 = of_int 1000

let make_prime_table_under_1000() =
    let prime_table = Array.make 1000 true in
    let rec fill_non_prime n =
	if n >= 1000 then
	    ()
	else if prime_table.(n) then begin
	    let rec continue i =
		if i * n >= 1000 then
		    ()
		else begin
		    prime_table.(i * n) <- false;
		    continue (i + 1)
		end
	    in
	    continue 2;
	    fill_non_prime (n + 1)
	end
	else
	    fill_non_prime (n + 1)
    in
    prime_table.(0) <- false;
    prime_table.(1) <- false;
    fill_non_prime 2;
    prime_table

(* table of primes < 1000 *)
let prime_table_under_1000 = make_prime_table_under_1000()

(** Given a positive integer less than 1000, this function returns true if the number is prime. *)
let is_prime_under_1000 n =
    assert (n < 1000);
    prime_table_under_1000.(n)

let print_prime_under_1000() =
    for n = 2 to 999 do
	if prime_table_under_1000.(n) then
	    Printf.printf "%d," n
    done;
    print_string "...\n";
    ()

let last_known_prime = of_int 997
let last_known_prime_square = square last_known_prime

let sort_raw_factor_list l = List.sort compare l

let make_factor_list l =
    let rec continue n c = function
	[] -> ( n, c )::[]
      |	m::l ->
	  if equal n m then
	      continue n (c + 1) l
	  else
	      ( n, c )::continue m 1 l
    in
    match sort_raw_factor_list l with
	[] -> []
      |	n::l -> continue n 1 l

let rec print_factor_list = function
    [] -> ()
  | ( p, 1 )::[] -> print_string (to_string p)
  | ( p, n )::[] -> Printf.printf "%s^%d" (to_string p) n
  | ( p, 1 )::l ->
      print_string (to_string p);
      print_string " * ";
      print_factor_list l
  | ( p, n )::l ->
      Printf.printf "%s^%d * " (to_string p) n;
      print_factor_list l
	  
let rec print_number_list = function
    [] -> ()
  | n::[] -> print_string (to_string n)
  | n::l ->
      Printf.printf "%s x " (to_string n);
      print_number_list l

(** Factorize a number by brute-force search. *)
let factorize_by_all_search n =
    assert (less n last_known_prime_square);
    let rec factorize n =
	if less_equal n last_known_prime && is_prime_under_1000 (to_int n) then
	    [n]
	else if mod2 n = 0 then
	    n2::factorize (shift_right n 1)
	else begin
	    let max = sqrt n in
	    let rec continue i =
		if less max i then
		    [n]
		else if is_prime_under_1000 (to_int i) then begin
		    let q, r = divmod n i in
		    if equal r zero then
			i::factorize q
		    else
			continue (add i n2)
		end
		else
		    continue (add i n2)
	    in
	    continue (of_int 3)
	end
    in
    if less_equal n one then
	( n, 1 )::[]
    else
	make_factor_list (factorize n)

(** make a quotient ring {i Z/nZ}. *)
let make_quotient_ring n =
    let barrett_reduction = F_big_int.make_barrett_reduction n in
    let adjust x =
        if less x zero then
	    add x n
	else if less x n then
	    x
	else
	    sub x n
    in
    let add_mod x y = adjust (add x y) in
    let sub_mod x y = adjust (sub x y) in
    let mul_mod x y = barrett_reduction (mul x y) in
    let square_mod x = barrett_reduction (square x) in
    let power_mod x n = mass_apply n mul_mod x one in
    add_mod, sub_mod, mul_mod, square_mod, power_mod

(*

(* Returns:
   true -> n is definitely a composite integer.
   false -> probable prime *)
let ascertain_composite_number_by_Miller_Rabin n =
    if less n n1000 then 
	not (is_prime_under_1000 (to_int n))
    else begin
	let add_mod, sub_mod, mul_mod, square_mod, power_mod = make_quotient_ring n in
	let nm1 = sub n one in
	let s, t =
	    let rec continue s t =
		if mod2 t = 0 then
		    continue (s + 1) (shift_right t 1)
		else
		    s, t
	    in
	    continue 0 nm1
	in
(*	      Printf.printf "n - 1 = 2^%d x %d = %d\n" s (f.to_int t) (f.to_int nm1); *)
	let check b =
(*		  Printf.printf "b = %d\n" (f.to_int b);  *)
	    let b = power_mod b t in
(*		  Printf.printf "b^%d = %d\n" (f.to_int t) (f.to_int b); *)
	    if equal b one then
		false
	    else begin
		let rec continue i b =
(*		    Printf.printf "b^(%d x 2^%d) = %d\n" (f.to_int t) i (f.to_int b); *)
		    if i = s then
			true
		    else if equal b nm1 then
			false
		    else
			continue (i + 1) (square_mod b)
		in
		continue 0 b
	    end
	in
	if check n2 then
	    true
	else if check n3 then
	    true
	else if check n5 then
	    true
	else if check n7 then
	    true
	else
	    false
    end

(* Pollard's p-1 method *)
let factorize_by_Pollard n =
    let add_mod, sub_mod, mul_mod, square_mod, power_mod = make_quotient_ring n in
    (* According to wiki (lol), B should be set to around n^(1/6). *)
    let b = sqrt (sqrt n) in
    let b = if less b n1000 then n1000 else b in
(*	  Printf.printf "B = %d\n" (f.to_int b); *)
    let rec continue p a =
	if p >= 1000 then
	    None
	else if equal a one then
	    None
	else if is_prime_under_1000 p then begin
(*		  Printf.printf "p = %d, " p; *)
	    let q = of_int p in
	    let k = log q b in
	    let m = power q (of_int k) in
(*		  Printf.printf "m = p^%d = %d, " k (f.to_int m);  *)
	    let a = power_mod a m in
(*		  Printf.printf "a^m = %d\n" (f.to_int a); flush stdout; *)
	    let am1 = sub a one in
	    let g = gcd am1 n in
(*		  Printf.printf "  g = %d\n" (f.to_int g); *)
	    if less_equal g one || equal g n then
		continue (p + 1) a
	    else
		Some g
	end
	else
	    continue (p + 1) a
    in
    let rec try_base p =
	if p >= 1000 then
	    None
	else if is_prime_under_1000 p then begin
(*		  Printf.printf "base = %d\n" p; *)
	    match continue 2 (of_int p) with
		Some g -> Some g
	      | None -> try_base (p + 1)
	end
	else
	    try_base (p + 1)
    in
    try_base 2

type 't decomposition_accuracy =
    TOTALLY_DECOMPOSED | POSSIBLY_COMPOSITE of 't list

let factorize_by_Pollard_Miller_Rabin n =
    let non_decomposable = ref [] in
    if less n zero then
	assert false
    else if less_equal n one then
	( n, 1 )::[], TOTALLY_DECOMPOSED
    else begin
	let rec factorize n =
(*	    Printf.printf "n = %s\n" (Varint2mod.to_string n); flush stdout; *)
	    if equal n one then
		[]
	    else if mod2 n = 0 then
		n2::factorize (shift_right n 1)
	    else if ascertain_composite_number_by_Miller_Rabin n then 
		match factorize_by_Pollard n with
		    None ->
		      Printf.printf "Warning: failed to factorize %s\n"
			  (to_string n);
		      non_decomposable := n::!non_decomposable;
		      []
		  | Some g ->
		      let q, r = divmod n g in
		      assert (equal r zero);
		      (factorize g)@(factorize q)
	    else
		[n]
	in
	let l = factorize n in
	let a =
	    match !non_decomposable with
		[] -> TOTALLY_DECOMPOSED | l -> POSSIBLY_COMPOSITE l
	in
	make_factor_list l, a
    end

(* Returns:
   true -> n is definitely a prime integer.
   false -> probable composite integer.
   Assume that n passed Miller-Rabin test. *)
let rec ascertain_prime_number_by_Pocklington_Lehmer n =
    let rec mul_list = function
	[] -> one
      |	n::l -> mul n (mul_list l)
    in
    let compute_nm1_div_p prime_list b p =
	let rec continue = function
	    [] -> b
	  | ( q, 1 )::l ->
	      if equal p q then
		  continue l
	      else
		  mul q (continue l)
	  | ( q, n )::l ->
	      if Varint2.equal p q then
		  mul (power q (of_int (n - 1))) (continue l)
	      else
		  mul (power q (of_int n)) (continue l)
	in
	continue prime_list
    in
    if less n zero then
	assert false
    else if less n n1000 then
	is_prime_under_1000 (to_int n)
    else begin
	let nm1 = sub n one in
	let sqrt_n = sqrt n in
	let p_power_nm1_cache = Array.create 1000 None in
	let add_mod, sub_mod, mul_mod, square_mod, power_mod = make_quotient_ring n in
	let power_mod a n =
	    power_mod (of_int a) n
	in
	let power_nm1 a =
	    match p_power_nm1_cache.(a) with
		Some b -> b
	      |	None ->
		  let b = power_mod a nm1 in
		  p_power_nm1_cache.(a) <- Some b;
		  b
	in
	let prime_list, composite_list =
	    let rec continue prime_list composite_list = function
		[] -> prime_list, composite_list
	      |	( p, n )::l ->
		  let rec expand n =
		      assert (n > 0);
		      if n = 1 then
			  p::composite_list
		      else
			  p::expand (n - 1)
		  in
		  if ascertain_prime_number_by_Pocklington_Lehmer p then
		      continue (( p, n )::prime_list) composite_list l
		  else
		      continue prime_list (expand n) l
	    in
	    match factorize_by_Pollard_Miller_Rabin nm1 with
		prime_list, TOTALLY_DECOMPOSED ->
		  continue [] [] prime_list
	      |	prime_list, POSSIBLY_COMPOSITE composite_list ->
		  continue [] composite_list prime_list
	in
	let b = mul_list composite_list in
(*	Printf.printf "n - 1 = %s\n" (Varint2mod.to_string nm1);
	print_string "prime: "; print_factor_list prime_list; print_newline();
	print_string "composite: "; print_number_list composite_list;
	Printf.printf " = %s\n" (Varint2mod.to_string b); flush stdout; *)
	let is_good_prime_factor p =
	    let nm1_div_p = compute_nm1_div_p prime_list b p in
	    let rec continue a =
		if a >= 1000 then
		    false
		else if is_prime_under_1000 a && equal (power_nm1 a) one then begin
		    let c = sub (power_mod a (nm1_div_p)) one in
		    let g = gcd c n in
		    if equal g one then begin
(*			Printf.printf "%s: %d^%s = 1 (mod N), gcd(%d^%s-1,N) = 1\n"
			    (Varint2mod.to_string p)
			    a (Varint2mod.to_string nm1)
			    a (Varint2mod.to_string nm1_div_p);  *)
			true
		    end
		    else
			continue (a + 1)
		end
		else
		    continue (a + 1)
	    in
	    continue 2
	in
	let greater ( x, _ ) ( y, _ ) = Varint2.compare y x in
	let prime_list = List.sort greater prime_list in
	let rec continue a = function
	    [] -> false
	  | ( p, n )::l ->
	      if is_good_prime_factor p then
		  let q = if n = 1 then p else power p (of_int n) in
		  let a = mul q a in
		  if less sqrt_n a then
		      true
		  else
		      continue a l
	      else
		  continue a l
	in
	continue one prime_list
    end



(*


let print l =
    print_factor_list l;
    print_newline()

let test n =
    factorize_by_Pollard_Miller_Rabin (Varint2.of_int n)

let is_composite n =
    ascertain_composite_number_by_Miller_Rabin (Varint2.of_int n)

let fact n =
    G.factorize_by_all_search (Varint2.of_int n)

let fact2 n =
    F.factorize_by_Pollard Varint2mod.make (Varint2.of_int n)

let is_prime n =
    ascertain_prime_number_by_Pocklington_Lehmer (Varint2.of_int n)

let is_prime2 n =
    match G.factorize_by_all_search (Varint2.of_int n) with
	[ _, 1 ] -> true
      |	_ -> false

let test() =
    let rec continue n =
	if n > 99999 then
	    ()
	else begin
	    let b = is_prime n = is_prime2 n in
	    Printf.printf "n = %d: " n;
	    print_string (if b then "ok\n" else "ng\n");
	    assert b;
	    continue (n + 2)
	end
    in
    continue 1031

let test2() =
    let a = Varint2.of_int 16 in
    let b = Varint2.of_int 3 in
    let c = Varint2.of_int 1048583 in
    let d = Varint2.of_int 2097169 in
    let n = Varint2.add (Varint2.mul (Varint2.mul a b) (Varint2.mul c d)) Varint2.one in
    Printf.printf "n = %s\n" (Varint2mod.to_string n); flush stdout;
    if ascertain_prime_number_by_Pocklington_Lehmer n then
	print_string "prime\n"
    else
	print_string "composite\n";
    ()
*)
*)
