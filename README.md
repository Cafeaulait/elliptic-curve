# Elliptic-Curve

A library for elliptic curve cryptography, which
provides a set of operations on
prime numbers, prime fields, and elliptic curves.
* The Miller-Rabin primarity test.
* The Pocklington-Lehmer primarity test.
* Pollard's p-1 algorithm.
* Factorization by Pollard-Miller-Rabin.
* add/sub/mul/div over prime fields.
* Inversion over prime fields.
* The Legendre symbol.
* Quadratic residue modulo n.
* Addition and multiplication over elliptic curves.

A point on an elliptic curve is represented by a projective coordinate system for efficiency.
Conversion functions from/to an affine coordinate system are also provided.
The test code includes the ElGamal public key encryption algorithm.

## Sample Code

Let us consider elliptic curve <i>y^2 = x^3 + 71x + 602</i> over finite field <i>F_1009</i>.
The curve has 1060 elements, so
for any element <i>p</i>, <i>1060p</i> should be zero.
The following code defines <i>F_1009</i>:

```ocaml
module F1009Spec = struct
  type element = Z.t
  let p = big_int_of_int 1009
end

module F1009 = F_big_int.Make (F1009Spec)
```

The elliptic curve <i>y^2 = x^3 + 71x + 602</i> is defined as follows:

```ocaml
module ECSpec1009 = struct
  type element = Z.t

  let a = big_int_of_int 71
  let b = big_int_of_int 602
end

module E1009 = Ec.Make (F1009) (ECSpec1009)
```

<i>( 1, 237 )</i> is a point on this curve.
We will compute 1060 times ( 1, 237 ).

```ocaml
let test() =
    let g = ( F1009.one, F1009.of_int 237, F1009.one ) in
    let x = E1009.multiply (big_int_of_int 1060) g in
    Printf.printf "[1060]g = %s\n" (E1009.string_of_point x);
    ()
```

The result should be zero (denoted by o).

## Benchmarks

NIST has recommended several elliptic curves
for Federal government use.
I have measured the elapsed times of
computing <i>the order times the base element</i>
over some of the NIST-recommended elliptic curves.
The result is as follows.

| Curve | Elapsed time |
|-------|--------------|
| P-192 | 0.993ms      |
| P-224 | 1.210ms      |
| P-521 | 4.545ms      |

(Core i7-4790K 4.00GHz)

## License

LGPL-3.0
