# IRIS
(which stand for "**I**nst**r**uct**i**on **s**~~election DSL~~")

An instruction selection DSL inspired by [ISLE](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/isle).
The main differences are:
- DSL syntax, I can't stand parenthesis soup. I love my curly brackets and semicolons
- there're no constructors/extractors, instead there's a single concept called terms
- term overloading
- first class support for tuple types
- IRIS doesn't support the other 80% of what ISLE has
