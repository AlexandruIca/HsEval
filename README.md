![HsEval](https://github.com/AlexandruIca/HsEval/workflows/HsEval/badge.svg)

# HsEval
Two simple parser for simple expressions. One can do:
```hs
evalPostfix "2 ^ (16 / 8)" -> 4.0
```
which first turns the input string into [a list of tokens](src/Tokenizer.hs), which is passed to [rpn](src/Parser.hs) which uses the [shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) to turn it into reverse polish notation. After that `evalPostfix` can directly interpret the 'stack' and compute the result. It can parse positive integers, `+`, `-`, `*`, `/`, `^` and of course paranthesis.

The other one can parse functions:
```hs
calc "sqrt(4 * 4)" -> 4.0
```
`calc` is a [recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser) parser implemented in [RecursiveDescent.hs](src/RecursiveDescent.hs) that turns the input string into an AST which is then evaluated recursively.

# Build
Make sure you have [stack](https://docs.haskellstack.org/en/stable/README/) installed.
```sh
stack build
```

# Run REPL
```sh
stack run
```

# Run tests
```sh
stack test
```
