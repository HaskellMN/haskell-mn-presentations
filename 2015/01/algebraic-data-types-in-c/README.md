# Algebraic Data Types in C
## Tagged Unions!

- [LYAH's introduction to ADTs](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types)

- [Functional C [pdf]](http://eprints.eemcs.utwente.nl/1077/02/book.pdf)

- Chapter 4.

### Sum and Product Types in Haskell

Anonymous products:

    x :: T * U
    x :: (T, U)
    x = (t, u)

Named products:

    z :: Point { x :: X, y :: Y }
    z = Point a b
    z = Point {x=a, y=b}

    -- lookup:
    x z
    y z


Named sums:

    data Shape = Circle a b c
               | Rectangle a b c d

Anonymous sums:

    ???


### Sum Types and Product Types in C

Anonymous Products:

    ???

Named products:

    typedef struct { X x; Y y } Point;
    Point z = {a, b};

    z.x
    z.y

Named sums:

    ...


Anonymous sums:

    typedef union { int i; double d } Number;
    Number n = 1;
    Number m = 1.0;

    // Bad access:
    n.d != m.d

So, back to named sums!
