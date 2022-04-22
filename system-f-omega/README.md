# system-f-omega

Terms and values:
```
t ::=
  x        // variable
  λx:T.t   // term abstraction
  t t      // application
  λX::K.t  // type abstraction
  t [X]    // type application
  true
  false
  if t then t else t
  let x=t in t
  {t_i}    // with i=1..n
  t.i

v ::=
  λx:T.t   // abstraction
  λX::K.t  // type abstraction
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
  ∀X::K.T // universal type
  λX::K.T // operator abstraction
  T T     // operator application

```

Kinds:
```
K ::=
  *      // kind of proper types
  K=>K   // kind of operators
```

## Examples

- the identity function(`(λX.λx:X.x)`) type-checks to `∀X.X->X`
- file `./quadruple.lambda`
- file `./existential-counter.lambda`