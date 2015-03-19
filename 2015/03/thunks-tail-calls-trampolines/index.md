---
title: Thunks, Tail calls, and Trampolines
author: Kyle Marek-Spartz
date: HaskellMN, March 18, 2015
---

# Cons

~~~ javascript
var cons = function (head, tail) {
  return function (f) {
    return f(head, tail);
  };
};
~~~


# Car

~~~ javascript
var car = function (pair) {
  return pair(
    function (head, tail) { return head; }
  );
};
~~~


# Cdr

~~~ javascript
var cdr = function (pair) {
  return pair(
    function (head, tail) { return tail; }
  );
};
~~~


# Streams

- Streams
- Lazy lists
- Possibly infinite


# Haskell Lists

~~~ haskell
ones = 1 : ones                              -- 1, 1, 1, ...
nats = 1 : map (1+) nats                     -- 1, 2, 3, ...
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)  -- 0, 1, 1, 2, 3, ...
~~~


# JavaScript...?

~~~ javascript
var ones = cons(1, ones); // "1undefined"
~~~


# Thunks!

~~~ javascript
var ones = cons(1, function () { return ones; });

var force = function (thunk) { return thunk() };
~~~

# JavaScript Streams

~~~ javascript
var streamCons = cons;
var streamHead = car;
~~~

# JavaScript Compose

~~~ javascript
var compose = function (f, g) {
  return function (_) {
    // arguments is a Javascript way of variadic arguments.
    var argsList = Array.prototype.slice.call(arguments);
    // Fn.apply takes a "this" context and a list of arguments:
    return f(g.apply(this, argsList));
  };
};
~~~

# JavaScript Streams, continued.

~~~ javascript
var streamTail = compose(force, cdr);
~~~

# Stream Map

~~~ javascript
var streamMap = function (stream, f) {
  return streamCons(
    f(streamHead(stream)),
    function () { return streamMap(streamTail(stream), f); }
  );
};
~~~

# Natural Numbers

~~~ haskell
nats = 1 : map (1+) nats
~~~

# ... in JavaScript

~~~ javascript
var increment = function (x) { return x + 1 };

var nats = streamCons(
  1,
  function () { return streamMap(nats, increment); }
);
~~~

# Stream ZipWith

~~~ javascript
var streamZipWith = function (s1, s2, f) {
  return streamCons(
    f(streamHead(s1), streamHead(s2)),
    function () { return streamZipWith(streamTail(s1), streamTail(s2), f); }
  );
};
~~~

# Fibonacci Stream

~~~ haskell
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
~~~

# ... in JavaScript

~~~ javascript
var sum = function (x, y) { return x + y; };

var fibs = streamCons(
  0,
  function () {
    return streamCons(
      1,
      function () { return streamZipWith(fibs, streamTail(fibs), sum); }
    );
  }
);
~~~

# Accessors

~~~ javascript
var second = compose(streamHead, streamTail);
var third = compose(streamHead, compose(streamTail, streamTail));
var fourth = compose(streamHead, compose(streamTail, compose(streamTail, streamTail)));
var fifth = compose(streamHead, compose(streamTail, compose(streamTail, compose(streamTail, streamTail))));
~~~


# Tail calls

~~~ javascript
var drop = function (stream, n) {
  if (n === 0) {
    return stream;
  }
  return drop(streamTail(stream), n - 1);
};
~~~

# Imperatively

~~~ javascript
var dropImperatively = function (stream, n) {
  while (n > 0) {
    stream = streamTail(stream);
    n = n - 1;
  }
  return stream;
};
~~~

# Trampoline Decorator

~~~ javascript
var trampoline = function (f) {
  return function (_) {
    var argsList = Array.prototype.slice.call(arguments);
    var result = f.apply(this, argsList);
    while (result instanceof Function) { // result is still a thunk
      result = force(result);
    }
    return result;
  };
};
~~~

# Using the Decorator

~~~ javascript
var _dropTrampolined = function (stream, n) {
  if (n === 0) {
    return stream;
  }
  return function () { return _dropTrampolined(streamTail(stream), n - 1); };
};

var dropTrampolined = trampoline(_dropTrampolined);
~~~

# Caching Thunks

~~~ javascript
var Thunk = function (item) { this.item = item };

Thunk.prototype.force = function () {
  if (this.item instanceof Function) {
    this.item = this.item();
  }
  return this.item;
};
~~~

# Caching Tail

~~~ javascript
var streamTail = function (stream) {
  return cdr(stream).force();
};
~~~

# Caching Map

~~~ javascript
var streamMap = function (stream, f) {
  return streamCons(
    f(streamHead(stream)),
    new Thunk(function () {
      return streamMap(streamTail(stream), f);
    })
  );
};
~~~

# Caching ZipWith

~~~ javascript
var streamZipWith = function (s1, s2, f) {
  return streamCons(
    f(streamHead(s1), streamHead(s2)),
    new Thunk(function () {
      return streamZipWith(streamTail(s1), streamTail(s2), f);
    })
  );
};
~~~

# Caching Fibs

~~~ javascript
var fibs = streamCons(
  0,
  new Thunk(function () {
    return streamCons(
      1,
      new Thunk(function () {
        return streamZipWith(fibs, streamTail(fibs), sum);
      })
    );
  })
)
~~~

# Caching Trampoline

~~~ javascript
var cachedTrampoline = function (f) {
  return function (_) {
    var argsList = Array.prototype.slice.call(arguments);
    var result = f.apply(this, argsList);
    while (result instanceof Thunk) {
      result = result.force();
    }
    return result;
  };
};
~~~

# Caching Drop

~~~ javascript
var _cachedDropTrampolined = function (stream, n) {
  if (n === 0) {
    return stream;
  }
  return new Thunk(function () {
    return _cachedDropTrampolined(streamTail(stream), n - 1);
  });
}

var cachedDropTrampolined = cachedTrampoline(_cachedDropTrampolined);
~~~
