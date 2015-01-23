#include <stdio.h>

typedef struct {
  double x;
  double y;
} Point;

typedef enum {
  CircleTag,
  RectangleTag
} ShapeTag;

typedef struct {
  Point p;
  double r;
} Circle;

typedef struct {
  Point topleft;
  Point topright;
} Rectangle;

typedef struct {
  ShapeTag tag;
  union {
    Circle c;
    Rectangle r;
  };
} Shape;

int main() {
  puts("Test.");
  return 0;
};
