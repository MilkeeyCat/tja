## Types
### Type declarations
Syntax:
```
extern type <type name> [= <external type name>];
```
Optional `<external type name>` allows to set a different type name that's going
to be used during code generation for the declared `<type name>`. If omitted,
the declared `<type name>` is used instead.

### Using types
Where a type is expected, you can specify either a type name or a tuple type.
Tuple types consist of comma-separated types enclosed in parentheses, i.e.
`(Type1, Type2, Type3)`.

### Builtin types
IRIS has a number of builtin types: `bool`, `i8`, `i16`, `i32`, `i64`, `isize`,
`u8`, `u16`, `u32`, `u64`, `usize`.

## Terms
There're 2 types of terms: concrete and overloaded. Concrete terms survive to
the final codegen, overloaded terms are resolved to concrete terms during
semantic analysis.

### Concrete terms
Syntax:
```
[extern] [pure] [partial | total | multi] term <term name>(<tys>) -> <ty>;
```
Optional `extern` keyword means whether the term is external or internal.<br>
Optional `pure` keyword means whether the term is pure, i.e. has no side effects.<br>
Keywords `partial`, `total`, `multi` describe the type of the term:<br>
- `partial` means the term returns 0 or 1 values
- `total` means the term returns 1 value
- `multi` means the term returns 0 or more values
`<tys>` is a list of types the term takes.<br>
`<ty>` is a return type of the term.

### Overloaded terms
Syntax:
```
overload term <term name> = <concrete term 1> [+ <concrete term n>];
```
Overload term declaration takes a list of concrete term names.

## Globals
Syntax:
```
extern let <global name>: <ty> = [const | field] <extern symbol name>;
```
`<ty>` is the type of the declared global.
Keywords `const`, `field` describe the type of the global:
- `const` means the global is a constant.
- `field` means the global is a non-constant immutable value, e.g., a struct field.
`<extern symbol name>` is the name of the symbol that's going to be used during
code generation for the declared `<global name>`.

### What do I do with them?
Globals can be used in patterns & expressions.

## Expressions
The following expressions are available:
- `foo` - identifiers
- `69` - integer literals. They can also have an optional type suffix, e.g. `69usize`
- `true/false` - booleans
- `(1, 2, 3)` - tuples
- `foo.0` - tuple indexes
- `foo(bar, baz)` - term calls

## Patterns
The following patterns are available:
- `bar` - identifiers
- `420` - integer literals. They can also have an optional type suffix, e.g. `420i16`
- `true/false` - booleans
- `(1, 2, 3)` - tuples
- `foo(bar, baz)` - term calls
- `_` - wildcards

## Rules
Syntax:
```
rule <term name>(<pats>) [guards] {
    [lets]

    <expr>
}
```
`<pats>` is a list of patterns for matching against `<term name>` return values.<br>
`guards` is an optional list of guards(syntax: `if let <pat> = <expr>`).
Guards are separated by `&&`.<br>
`lets` is an optional list of let definitions(syntax: `let <var>: <ty> = <expr>;`).<br>
`<expr>` is the value that's returned after matching the rule.

### Rule fun facts
1. Rule is matching on term's return value, not arguments. If the return type is
a tuple, it's expanded, so if a term's return type is `u32`, rule takes 1
pattern, but if the type is `(u32, usize)`, rule takes 2 patterns.
2. The first occurrence of an identifier binds it to an expression. Subsequent
occurrences match only if their expressions are equal to the expression bound to
that identifier.
3. Guards can use only pure terms.
