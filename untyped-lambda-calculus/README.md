# untyped-lambda-calculus

## Examples

Trying Church booleans:
- `tru` = `λt.λf.t`
- `fls` = `λt.λf.f`
- `and` = `λb.λc.b c fls`

`and tru tru` evaluates to `tru`:
```
enter term:
(\b.\c.b c \t.\f.f) (\t.\f.t) (\t.\f.t)
Term before evaluation: (λb.(λc.b c (λt.(λf.f)))) (λt.(λf.t)) (λt.(λf.t))
Term after evaluation: (λt.(λf.t))
```

`and tru fls` evaluates to `fls`:
```
enter term:
(\b.\c.b c \t.\f.f) (\t.\f.t) (\t.\f.f)
Term before evaluation: (λb.(λc.b c (λt.(λf.f)))) (λt.(λf.t)) (λt.(λf.f))
Term after evaluation: (λt.(λf.f))
```