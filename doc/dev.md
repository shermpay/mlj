# Development Info

## Overview
A DSL that offers ML like semantics and syntax such as the type system, pattern
matching, currying etc.

## Details
- ML like functions
- ML like Hindley Milner static type system

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

Phases can be viewed as following:
mlj forms --(mlj.compiler)--> clojure mlj.lang macros --(type-checker)--(mlj.lang)--> clojure.core forms

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
> This might actually be feasible!
Translate #a to :a? Index Tuples/Records
> This would be more complex, but would be really nice if achievable

#### Macro Transformation phase
Utilizing Clojure Macros to construct valid Clojure Builtin Forms.
This phase consists of 2 components:
* Transformation
* Type Checker

### Type System
*Type System* should be completely **static**, **Hindley Milner**.
> Not sure if achievable, but this mimics SML the most.

The *Type System* is part of the second macro transformation phase to achieve staticness.
Two possible ideas to type checking.
* ML functions as Clojure macros which include a type checker.
* A macro that inspects the namespace, and type checks every form.
> Both approach seem fairly complex, the second one seems more extensible and feasible.
> Currently using the second approach

The *Type Checker* should be part of the compiler, so the compiler comprises of the following:
* Parser
* Type checker

*Type System* will be implemented as such:
1. It will obtain type information of declared dependencies via the *meta-data* `:type` tag of
the Clojure `var`.
2. Build up a `type-map` of {`var` => type}.
3. Iterate through the compiled expressions/declaration while doing the following:
	- Type checks each form
	- Adds to the `type-map` whenever it encounters a declaration
	- Replace the `type-declaration` into a lower level form

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

## Road Map
### Syntax
#### `fn`
`fn param :T => body`
- param is a single parameter or a tuple of parameters
- :T is a Clojure keyword representing an MLJ type.
- body is a expression
#### `fun`
`fun name param :T = body`  
- param is a single parameter or a tuple of parameters (No sugared currying
support yet
- :T is a Clojure keyword representing an MLJ type.
- body is an expression
#### `val`
`val name :T = expr`  
- :T is a Clojure keyword representing an MLJ type.
- expr is an expression
#### `let`
`let val name :T = expr in body end`
This goes through a transformation via compile into `let [val name :T = expr] in
body end`
#### `if`
`if pred then expr1 else expr2`
- pred, expr1, expr2 all can be expressions
#### `case`
`case expr of body`
Not a lot of focus has been put into pattern matching

### Type System
Type checking implemented for all Syntax form excep `case`. Done after
compilation-1, before compilation-2. No forms of type inference, but does build
type environments for extensive type checking of variable usage.

### Tests
Unit tests for `lang`, `type`, `core` namespace.

### Todos
- Work on generic types
- Work on stdlib
- Work on `compile` namespace
- Learn about `Local Type Inference`, attempt to implement it with `core.logic`
