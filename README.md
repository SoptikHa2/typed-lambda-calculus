# Simply typed lambda calculus type checker

- Create a program in Haskell, that will type check given lambda calculus
expression, which may be (partially) annotated by user.
- Let the type checker infer types, where feasible

## Example usage

`make run`

Type lambda expression (such as `(/tf.t)`. Afterwards, actions can be executed on it.
It can be desugared `:d`, normalized (and desugared in the process) `:n N`/`:c`, or
type annotated `:t [CONTEXT]`. See `:h` for additional info.

Common abbreviations such as `Y` for Y-combinator are built in.

Multi-letter variables are possible, but have to be always typed in uppercase, and be
fully enclosed in an extra pair of parenthesis.
