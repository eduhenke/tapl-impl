# system-f-sub

Terms and values:
```
t ::=
  x        // variable
  λx:T.t   // term abstraction
  t t      // application
  λX<:T.t  // type abstraction
  t [X]    // type application
  true
  false
  if t then t else t
  let x=t in t
  {t_i}    // with i=1..n
  t.i

v ::=
  λx:T.t
  λX<:T.t
  true
  false
  {v_i}    // with i=1..n
```

Types:
```
T ::=
  Bool
  Nat
  Top
  T -> T
  {T_i}   // with i=1..n
  X       // type variable
  ∀X<:T.T
```

## Examples

- the identity function(`(λX.λx:X.x)`) type-checks to `∀X.X->X`
- file `./quadruple.lambda`
- file `./existential-counter.lambda`