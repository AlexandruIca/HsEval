# HsEval
A simple parser for simple expressions. Currently it can do:
```hs
evalPostfix "2 ^ (16 / 8)" -> 4.0
```
which first turns the input string into [a list of tokens](src/Tokenizer.hs), which is passed to [rpn](src/Parser.hs) which uses the [shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) to turn it into reverse polish notation. After that `evalPostfix` can directly interpret the 'stack' and compute the result.

It can parse positive integers, `+`, `-`, `*`, `/`, `^` and of course paranthesis.

#### TODO
* Constants
* Functions
* AST generation
* AST interpreter
* Bytecode generation/interpretation

# Build
Make sure you have [stack](https://docs.haskellstack.org/en/stable/README/) installed.
```sh
stack build
```

# Run example
```sh
stack run
```

# Run tests
```sh
stack test
```