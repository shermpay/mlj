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

Both will use the same Literal syntax as the Clojure counter parts.

### Functions
`fn` as a way to define *Anonymous Functions*.  
`fn` only accepts a single parameter, either a value or a `Record`|`Tuple`.  
Therefore functions should be defined with the following syntax `fn param => body`.

