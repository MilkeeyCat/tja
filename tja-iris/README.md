# IRIS
(which stand for "**I**nst**r**uct**i**on **s**~~election DSL~~")

An instruction selection DSL inspired by [ISLE](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/isle).
The main differences are:
- DSL syntax, I can't stand parenthesis soup. I love my curly brackets and semicolons
- there're no constructors/extractors, instead there's a single concept called terms
- term overloading
- first class support for tuple types
- IRIS doesn't support the other 80% of what ISLE has

## Resources used during development
- <https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching>
- <http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf>
- <http://moscova.inria.fr/~maranget/papers/warn/warn.pdf>
- <https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/isle>
