var cons = function (head, tail) {
  return function (f) { return f(head, tail); };
};

var car = function (pair) {
  return pair(
    function (head, _) { return head; }
  );
};

var cdr = function (pair) {
  return pair(
    function (_, tail) { return tail; }
  );
};

var range = function (n) {
  var l = [];
  for (var i = 0; i < n; i++) {
    l.push(i);
  }
  return l;
};

var Vector = {
  size: car,
  contents: cdr,

  lookup: function (n, vector) {
    var lookupWithSize = function (n, subvector, size) {
      if (n >= size || n < 0) {
        return null;
      }

      if (size === 1) {
        return subvector;
      }

      var newSize = Math.floor(size / 2);

      var descendLeft = car;
      var descendRight = cdr;

      if (n < newSize) {
        return lookupWithSize(n, descendLeft(subvector), newSize);
      } else {
        return lookupWithSize(n - newSize, descendRight(subvector), newSize);
      }
    };

    return lookupWithSize(n, Vector.contents(vector), Vector.size(vector));
  },

  toList: function (vector) {
    return range(Vector.size(vector)).map(function (n) { return Vector.lookup(n, vector); });
  }
}

var emptyVector = cons(0, null);

var exampleVector = cons(8,
  cons(
    cons(
      cons(0, 1),
      cons(2, 3)
    ),
    cons(
      cons(4, 5),
      cons(6, 7)
    )
  )
);
