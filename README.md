# tapl-impl

This repository is meant to be a personal collection of implementations of the concepts from the TaPL(Types and Programming Languages) book.

Each folder contains a parser+interpreter of a given language. You just need to enter in the repository and `cabal run tapl`.

## Implemented type systems

1. [Untyped lambda calculus](./untyped-lambda-calculus)(Chapter 5, 6, 7)
2. [Simply Typed Lambda Calculus(with extensions: records/tuples, variant/sum, etc.)](./simply-typed-lambda-calculus)(Chapter 9, 10, 11)
3. [Simply Typed Lambda Calculus with subtyping](./simply-typed-with-subtyping)(Chapter 15, 16)
4. [Simply Typed Lambda Calculus with recursive types](./simply-typed-with-recursive-types)(Chapter 20, 21)
5. [Simply Typed Lambda Calculus with type reconstruction(type inference)](./simply-typed-with-type-reconstruction/)(Chapter 22)
6. [System F](./system-f)(Chapter 23, 24, 25)
7. [System F with subtyping(Bounded Quantification)](./system-f-sub)(Chapter 26, 28)
8. [System F omega(with type operators)](./system-f-omega)(Chapter 29, 30)

## Type Systems Features

The main type systems features covered on TaPL are:

- **Types**: allows you to classify the possible runtime values of terms, e.g. a variable of type `Nat`, can only hold a natural number at runtime(e.g. 0, 1, 5, etc.).
- **Subtyping**: allows you to have a more specific type than a more general one, i.e. given a type `S`, which is a subtype of the type `T`, all possible runtime values of `S` will be within all the possible runtime values of `T`, e.g. `Nat`(0, 1, 2, ...) is a subtype of `Integer`(-2, -1, 0, 1, 2, ...). We can use this feature on functions and variables, e.g. a function that needs a `Integer` as an argument, can also take a variable of type `Nat` as an argument, because `Nat` is a subtype of `Integer`.
- **Recursive types**: allows you to represent infinite data structures, e.g. a list of something(`data NatList = Nil | Cons Nat NatList`), a tree of something, etc.
- **Type inference**: allows you to omit the type annotations on your code, the compiler will infer the type of each term, e.g. `λn: succ (succ n)`(instead of `λn:Nat. succ n`) `n` will be inferred to type `Nat`, because the compiler knows that `n` must be a natural number, since we're applying the `succ` function on it, likewise with `λb: if b then 0 else 2`, `b` will be inferred to `Bool`, with no added type annotations.
- **Polymorphism(or more formally "Parametric polymorphism")**: allows you to write generic functions(that operate on **any** type), e.g. `let id=(λT. λt:T. t)`, where the first argument is a type argument, and the second one is a term argument, with that we can use the same function on multiple types: `id Nat 0` will be evaluated to `0`, `id Bool true` will be evaluated to `true`, etc.
- **Higher-order polymorphism(or type operators)**: allows you to write generic functions **on types**, e.g. we can define the `Pair` type constructor(or kind), and we can feed this type constructor with two type arguments to yield a new type, e.g. `Pair Nat Bool` symbolizes the **type** of a pair of a natural number and a boolean value.

## Table

| System                                                            | subtyping | recursive types | type inference | polymorphism(term expressions) | higher-order polymorphism(type expressions) |
| ----------------------------------------------------------------- | --------- | --------------- | -------------- | ------------------------------ | ------------------------------------------- |
| [S.T.(Simply Typed)](./simply-typed-lambda-calculus)              | -         | -               | -              | -                              | -                                           |
| [S.T. w/ subtyping]((./simply-typed-with-subtyping))              | X         | -               | -              | -                              | -                                           |
| [S.T. w/ recursive](./simply-typed-with-recursive-types)          | -         | X               | -              | -                              | -                                           |
| [S.T. w/ reconstruction](./simply-typed-with-type-reconstruction) | -         | -               | X              | -                              | -                                           |
| [System F](./system-f)                                            | -         | -               | -              | X                              | -                                           |
| [System F-sub](./system-f-sub)                                    | X         | -               | -              | X                              | -                                           |
| [System F-omega](./system-f-omega)                                | -         | -               | -              | X                              | X                                           |

