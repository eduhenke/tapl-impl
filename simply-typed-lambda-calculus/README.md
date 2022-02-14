# simply-typed-lambda-calculus

Terms and values:
```
t ::=
  x        // variable
  \x:T.t   // abstraction
  t t      // application
  true
  false
  if t then t else t

v ::=
  \x:T.t   // abstraction value
```

Types:
```
T ::=
  Bool
  T -> T
```

## Examples

Trying with booleans:
- `and` = `λx:Bool.λy:Bool. if x then y else false`

`and true true` evaluates to `true`:
```
enter term:
(\x:Bool.\y:Bool. if x then y else false) true false
Term before evaluation: (λx:Bool.(λy:Bool.if x then y else false)) true true
Succesfully typechecked: Bool
Term after evaluation: true
```

`and true false` evaluates to `false`:
```
enter term:
(\x:Bool.\y:Bool. if x then y else false) true false
Term before evaluation: (λx:Bool.(λy:Bool.if x then y else false)) true false
Succesfully typechecked: Bool
Term after evaluation: false
```

`and true (λx:Bool.x)` doesn't type check:
```
(\x:Bool.\y:Bool. if x then y else false) true (\x:Bool.x)
Term before evaluation: (λx:Bool.(λy:Bool.if x then y else false)) true (λx:Bool.x)
Expression does not typecheck: TypeAppArgumentMustMatch
```