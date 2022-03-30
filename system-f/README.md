# system-f(with records)

Terms and values:
```
t ::=
  x        // variable
  λx:T.t   // term abstraction
  t t      // application
  λX       // type abstraction
  t [X]    // type application
  true
  false
  if t then t else t
  let x=t in t
  {t_i}    // with i=1..n
  t.i

v ::=
  λx:T.t
  λX.t
  true
  false
  {v_i}    // with i=1..n
```

Types:
```
T ::=
  Bool
  Nat
  T -> T
  {T_i}   // with i=1..n
  X       // type variable
  ∀X.T
```

## Examples

- the identity function(`(λX.λx:X.x)`) type-checks to `∀X.X->X`
- file `./quadruple.lambda`
- file `./existential-counter.lambda`