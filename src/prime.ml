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

(* Given a positive integer less than 1000, this function returns true if the number is prime. *)
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

(** Factorizes a positive integer by brute-force search. *)
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

(** Makes a quotient ring {i Z/nZ}. 
    [make_quotient_ring n] returns a tuple of 5 operators ( add, sub, mul, square, power ). *)
(* dynamic module might be better. *)
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

(** The Miller-Rabin primarity test.
    Given {i n}, if this function returns true {i n} is definitely a composite number.
    Otherwise, {i n} is a probable prime. *)
let is_composite_number_by_Miller_Rabin n =
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
	let check b =
	    let b = power_mod b t in
	    if equal b one then
		false
	    else begin
		let rec continue i b =
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

(** Pollard's {i p-1} algorithm. *)
let factorize_by_Pollard n =
    let add_mod, sub_mod, mul_mod, square_mod, power_mod = make_quotient_ring n in
    let b = sqrt (sqrt n) in
    let b = if less b n1000 then n1000 else b in
    let rec continue p a =
	if p >= 1000 then
	    None
	else if equal a one then
	    None
	else if is_prime_under_1000 p then begin
	    let q = of_int p in
	    let k = log q b in
	    let m = power q (of_int k) in
	    let a = power_mod a m in
	    let am1 = sub a one in
	    let g = gcd am1 n in
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

(** Factorization by Pollard-Miller-Rabin. *)
let factorize_by_Pollard_Miller_Rabin n =
    let non_decomposable = ref [] in
    if less n zero then
	assert false
    else if less_equal n one then
	( n, 1 )::[], TOTALLY_DECOMPOSED
    else begin
	let rec factorize n =
	    if equal n one then
		[]
	    else if mod2 n = 0 then
		n2::factorize (shift_right n 1)
	    else if is_composite_number_by_Miller_Rabin n then 
		match factorize_by_Pollard n with
		    None ->
		      Printf.printf "Warning: failed to factorize %s\n" (to_string n);
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

(** The Pocklington-Lehmer primarity test.
    Given {i n}, if this function returns true, then {i n} is a prime.
    Otherwise, {i n} is a probable composite number.
    Assume that {i n} has passed Miller-Rabin test. *)
let rec is_prime_number_by_Pocklington_Lehmer n =
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
	      if equal p q then
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
	let p_power_nm1_cache = Array.make 1000 None in
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
		  if is_prime_number_by_Pocklington_Lehmer p then
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
	let is_good_prime_factor p =
	    let nm1_div_p = compute_nm1_div_p prime_list b p in
	    let rec continue a =
		if a >= 1000 then
		    false
		else if is_prime_under_1000 a && equal (power_nm1 a) one then begin
		    let c = sub (power_mod a (nm1_div_p)) one in
		    let g = gcd c n in
		    if equal g one then begin
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
	let greater ( x, _ ) ( y, _ ) = compare y x in
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
