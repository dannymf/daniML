# DaniML (ProbaML) A Functional Language Supporting Probabilistic Computation

## Purpose

## Types

- `int`
- `bool`
- `float` (real number)
- `prob [type]` (probability distribution)
- `[type] -> [type]` (function)

## Expressions

- Let bindings: `let [var] = [expr] in [expr]`
- If-then-else statements
- Binary operators: `+`, `-`, `*`, `<=`
- Anonymous functions: `fun [var] : [type] => [expr]`
  - Only provide the type of the argument, not the function
- Recursive functions: `let fix [fname] : [type] = fun [var] : [type] => [expr]`
  - **Important notes:**
  - Every recursive function must contain at least one argument (the `fun [var] : [type]` after the `=` is required)
  - Examples: (single argument)
  ```ocaml
  let multi_arg = fun x:int => fun y:int =>
      let fix factorial : int -> int =
        fun n:int => if n <= 1 then 1 else n * factorial (n - 1)
        in factorial x + y in multi_arg 5 7
  ==================>
  127
  ```
  - Alternatively, for a recursive function that itself has multiple arguments, try this: (Multi argument)
  ```ocaml
  let fix double_factorial : int -> int -> int =
        fun x:int => fun y:int =>
          if x <= 1 then
            if y <= 1 then 1
            else y * double_factorial 1 (y-1)
          else x * double_factorial (x-1) y
        in double_factorial 5 10
  ==================>
  25401600
  ```
- Random sampling: `S`
- Probability distributions: `prob [expr]`
- Sample from probability distributions: `sample [var] from [prob p] in [expr]`

## Building

- Run `make` to build the project
- Run `make repl` to use the interactive interpreter
- Run `make test` to run the testing suite
- Edit `run.ml` and run `make run` to run a computation repeatedly
- Add a file with ProbaML code and use
  `make interpret n=<NUMBER_OF_TIMES> fname=<FILENAME>` to run the code

## Examples

- See the `examples` folder
