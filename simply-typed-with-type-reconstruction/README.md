# simply-typed-with-type-reconstruction

Terms and values:
```
t ::=
  x        // variable
  \x:T.t   // abstraction
  \x.t     // abstraction implicitly typed
  t t      // application
  true
  false
  if t then t else t
  let x=t in t

v ::=
  \x:T.t   // abstraction value
  true
  false
```

Types:
```
T ::=
  Bool
  Nat
  T -> T
```

## Examples

- `λz.λy. z (y true)`: Type-checks(and reconstructs type) to `(?X2->?X3)->(Bool->?X2)->?X3`
- Let-polymorphism works(using double both with `Nat` and `Bool` types):
```
let double=(λg.λx.g (g x)) in
let fNat=(λn. succ n) in
let fBool=(λb. if b then false else true) in
let n=(double fNat 5) in
if (double fBool true) then n else 0
```