% Rust ♥'s Haskell

Andrew Clarkson

January 21, 2015

# Rust is...

A systems programming language designed to be:

- fast
- concurrent
- safe

# Origin Story

Why create another language?

# Haskell ↔ Rust

What has Rust "stolen" from languages like Haskell?

- Type Safety
- Immutability
- First Class Functions
- Pattern Matching
- Destructuring
- Traits
- Option
- Deriving
- Type Kinds

# Type Safety

Errors at compile time > errors at runtime

No `void *`!

# Immutability

Variables are immutable by default

```ignore
let count: int = 0;
count += 1; // Error
```

# But...

People love mutable state

```rust
let mut count: int = 0;
count += 1;
```

# Functions

Functions are first class citizens

```rust
use std::iter::AdditiveIterator;
use std::iter;

fn main() {
    let bound: u32 = 1000;
    let sum: u32 =
        iter::count(0, 1).
        map(|n| n * n).
        take_while(|&n| n < bound).
        filter(|n| is_odd(*n)).
        sum();
}

fn is_odd(n: u32) -> bool {
    n % 2 == 1
}
```

# Pattern Matching

Like `case ... of`

```
let number: u32 = 3000;
let message = match number {
    1 => "one",
    2 | 3 | 4 => "other numbers",
    5 ... 10 => "moar numbers",
    _ => "I can't count that high",
};
```

# Pattern Matching (cont'd)

With guards

```
let number: u32 = 3000;
let message = match number {
    1 => "one",
    _ if number % 2 == 0 => "an even number",
    _ => "an odd number",
};
```

# Destructuring

Both structures and tuples.

```
type Point = (f64, f64);
struct Centroid { count: u32, point: Point }

fn add_point(centroid: Centroid, point: Point) -> Centroid {
    let (x2, y2) = point;
    let Centroid { count: _, point: (x1, y1)} = centroid;
    Centroid {
        count: centroid.count + 1,
        point: ((x2 + x1) / 2.0, (y2 + y1 / 2.0)),
    }
}

fn main() {
    let centroid = Centroid {
        count: 1,
        point: (-2.0, 3.0),
    };
    
    add_point(centroid, (1.0, 3.0));
}

```

# Traits

Like type classes

```
trait Barking {
    fn bark(&self);
}

fn make_bark<T: Barking>(barker: T) {
    barker.bark();
}

struct Dog;

impl Barking for Dog {
    fn bark(&self) {
        println!("Bark");
    }
}

```

# Option

`Option` = `Maybe`
`Some` = `Just`
`None` = `Nothing`

```rust
fn divide(numerator: i32, denominator: i32) -> Option<f64> {
    match denominator {
        0 => None,
        _ => Some(numerator as f64 / denominator as f64),
    }
}
fn main() {
    match divide(5, 2) {
        None => println!("Failed"),
        Some(result) => println!("{}", result),
    }
}
```

# Deriving

Make the compiler write our code

```
use std::string::String;

#[deriving(Show, PartialEq, Eq)]
struct Person {
    name: String,
    age: u32,
}
```

# Type Kinds

Sets of Types like:

- Sized
- Copy
- Send

