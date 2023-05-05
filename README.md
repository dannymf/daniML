# DaniML (ProbaML) A Functional Language Supporting Probabilistic Computation

## Purpose

## Types

- `int`
- `bool`
- `float` (real number)
- `prob [type]` (probability distribution)
- `[type] -> [type]` (function)
-

## Expressions

- Let bindings: `let [var] = [expr] in [expr]`
- Anonymous functions: `fun [var] : [type] => [expr]`
  - Only provide the type of the argument, not the function
- Recursive functions: `let fix [fname] : [type] = fun [var] : [type] => [expr]`
  - **Important notes:**
  - Every recursive function must contain at least one argument (the `fun [var] : [type]` after the `=` is required)
  - To implement multi-argument recursive functions, you must put the additional arguments **before** the let binding. For example,
  ```
  let multi_arg = fun x:int => fun y:int =>
      let fix factorial : int -> int =
        fun n:int => if n <= 1 then 1 else n * factorial (n - 1)
        in factorial x + y in testtest 5 7
        =====>
  127
  ```
- Random sampling: `S`
- Probability distributions: `prob [expr]`
- Sample from probability distributions: `sample [var] from [prob p] in [expr]`

## Building

- Run `make` to build the project
- Run `make repl` to use the interactive interpreter
- Run `make test` to run the testing suite
- Edit `run.ml` and run `make run` to run a computation repeatedly
