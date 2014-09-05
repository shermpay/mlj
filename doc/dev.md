# Development Info

## Overview
A DSL that offers ML like semantics and syntax such as the type system, pattern
matching, currying etc.

## Details
- ML like Tuple/Record types
- ML like functions

### Tuple/Record types
`Records` are just a wrapper for Clojure `Maps`  
`Tuples` are just a wrapper for Clojure `Vectors`  
> Not too sure about this anymore. Depends on implementation of "Compiler"

Both will use the same Literal syntax as the Clojure counter parts.

### Functions
`fn` as a way to define *Anonymous Functions*.  
`fn` only accepts a single parameter, either a value or a `Record`|`Tuple`.  
Therefore functions should be defined with the following syntax `fn param => body`.
Functions should be defined withe a type-checker at the top of the function, and the definition should be type
checked as well.

### Symbols
Symbols should be assigned types of some form

### Compiler
A very lightweight compiler, current idea is to parenthesize
expressions. Most builtin keywords hav a start and end delimiter.
Example:
```sml
if true then
1
else
2
```  
Can be translated to
```clojure
(if true 1 2)
```

Translate () to []? Tuples as Vectors
Translate #a to :a? Index Tuples/Records

## Implemented Functionality
#### Keywords
- `fn`
- `fun`
- `val`
- `let`
- `case`
- `if`
