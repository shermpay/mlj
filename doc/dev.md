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
Multiple components comprising of *2* major phases.
#### Clojure Compilation
A very lightweight compiler, current idea is to parenthesize
expressions and transform Expressions into S-Expressions recognized by Clojure.  
> (Building String representation of ASTs)

Another way is to create a huge macro called sml and wrap everything into it.
And to **compile** the program is to just wrap everything in it.
This approach is challenging in ways, but the benefits seem huge. Because within
the macro, I would be able to do complex transformations, type checking etc.

Example: ```sml if true then 1 else 2
```  
Can be translated to
```clojure
(if true 1 2)
```

Translate () to []? Tuples as Vectors
Translate #a to :a? Index Tuples/Records

#### Macro Transformation phase
Utilizing Clojure Macros to construct valid Clojure Builtin Forms.
This phase consists of 2 components:
* Transformation
* Type Checker

### Type System
*Type System* should be completely **static**, **Hindley Milner**.
> Not sure if achievable, but this mimics SML the most.

The *Type System* is part of the second macro transformation phase to achieve staticness.
> Plan of attack! Built the type system with zero inference.
Two possible ideas to type checking.
* ML functions as Clojure macros which include a type checker.
* A macro that inspects the namespace, and type checks every form.
> Both approach seem fairly complex, the second one seems more extensible and feasible.

#### Type Declarations
Any where a binding can be introduced
- `let`
Syntax
```sml
let
	[val x :T = expr]
in
	x
end
```
Type checking expr to match type T, then erase the types.

## Implemented Functionality
#### Keywords
- `fn`
- `fun`
- `val`
- `let`
- `case`
- `if`
