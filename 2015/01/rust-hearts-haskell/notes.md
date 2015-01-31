% Rust ♥'s Haskell

# Rust is...

Really good at:
- Hard real time systems
- Programmers that care about that last 10% (aka systems people)
- Safety (making sure if it compiles it will run)

# Origin Story

Why create another language?

Tries to move away from the control vs safety tradeoff.

```
High control -------------------- Low control
Low Safety   -------------------- High safety
  C / C++    --------------------    Ruby
```

Safety is maintained via a really good compiler, while giving the programmer low-level control

# Haskell ↔ Rust

Many (at least a few) of the core Rust team are Haskellers.

What has Rust "stolen" from languages like Haskell?

- Type Safety 
- Immutability -- to some extent??
- First Class Functions 
- Pattern Matching -- and/or ML?
- Destructuring -- and/or ML?
- Traits -- like type classes
- Option -- like Maybe
- Deriving
- Type Kinds -- higher kinded types

# Type Safety

The underlying philosophy is much like Haskell's that errors at compile time are always preferable to errors at runtime.

# Immutability

Variables are immutable by default which may discourage immutable state

# But...

mutable state is really familiar to systems programmers and thus can be added via the `mut` keyword.

# Functions

Functions are first class citizens and can be passed as arguments and defined within other functions.

Yay closures!!

# Pattern Matching

Almost exactly like `case ... of`

Checks for completeness which is really nice!

# Pattern Matching (cont'd)

Guards are not, I repeat not checked for completeness. 

*My error was kindly pointed out by some very smart haskellers* :)

# Destructuring

almost exactly the same as Haskell and Ocaml and maybe ML?

# Traits

Rust doesn't have "classes" like C++ or Java but instead uses traits which behave like Haskell's type classes

This separates data from functionality (methods) and allows for "Zero cost abstractions"

# Option

Corresponds very well with haskell

`Option` = `Maybe`
`Some` = `Just`
`None` = `Nothing`

# Deriving

Let's make the compiler write our code

Because programmers are lazy even if their languages are not!

# Type Kinds

Type kinds can only be implemented within the compiler... so far


#Not covered in the slides:

- safety can be explicitly ignored like perform unsafe IO using unsafe blocks
- unsafe functions can only be called in unsafe blocks but it __does not propogate upwards__ 
- Rust primarily uses the stack "unboxed" while in Haskell everything is "boxed" and uses the heap.
- In rust you can "Box" values to store them on the stack. They are freed using some smart compiler magic (RAII junk)

