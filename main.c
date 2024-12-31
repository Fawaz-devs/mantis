

#include <stdio.h>
struct Foo {
  int a;
  int b;
  int c;
  long d;
  long e;
};

struct FourLongs {

  long a;
  long b;
  long c;
  long d;
};

union {
  struct FourLongs longs;
  struct Foo foo;
} FooOr4Longs;

extern struct Foo create_foo(int b);
extern long sum_foo(struct Foo foo);
extern long sum_foo_ptr(struct Foo *foo);
extern long anonymous_fn(long foo);


long my_sum(struct Foo foo) {
  printf("My Sum Foo Addr: %ld\n", (long)&foo);
  return sum_foo_ptr(&foo);
}


extern void print_foo(struct Foo* f) {
  // struct Foo* foo = &f;
  struct Foo* foo = f;
  printf("Foo { a: %d, b: %d, c: %d, d: %ld, e: %ld }\n", foo->a, foo->b,
         foo->c, foo->d, foo->e);

  struct Foo nf = *(struct Foo*)&f;
  foo = &nf;
  printf("Foo { a: %d, b: %d, c: %d, d: %ld, e: %ld }\n", foo->a, foo->b,
         foo->c, foo->d, foo->e);
}

int main() {

  struct Foo foo = create_foo(0);
  // print_foo(&foo);

  // struct FourLongs longs = *(struct FourLongs*)&foo;
  // long sum = sum_foo(longs.a, longs.b, longs.c, longs.d);
  // long sum = sum_foo(foo);
  // long addr_foo = (long)&foo;
  // printf("sum of <%ld>foo %ld, deref = %d\n", addr_foo, sum, *((int*)sum + 2));
  // // sum = sum_foo_ptr(&foo);
  // // printf("sum of foo ptr %ld\n", sum);
  // sum= my_sum(foo);
  // printf("sum of my foo %ld\n", sum);

  long sum= anonymous_fn(10);
  printf("sum of my foo %ld\n", sum);

  return 0;
}
