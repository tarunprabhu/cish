#include <stdbool.h>
#include <stdlib.h>

struct Struct {
  int a;
  long *b;
  char pad[4];
  struct Inner {
    bool x;
    long double g;
  } inner;
};

union Union {
  long l;
  int i;
  char c;
};

struct Empty {
  ;
};

struct SOA {
  int i;
  struct Struct s[3];
};

struct Empty* return_ptr() {
  return NULL;
}

void pass_by_value(struct Struct s) {
  srand(s.a);
}

struct Struct return_by_value(struct Struct* s) {
  return *s;
}

long* return_local_field(int a, long *b, bool x, long double g) {
  struct Struct s = {a, b, "Wow", {x, g}};
  return s.b;
}

bool get_param_field(struct Struct* s) {
  return s->inner.x;
}

long* get_param_ptr_field(struct Struct* s) {
  return s->b;
}

long get_param_deref_field(struct Struct* s) {
  return *s->b;
}

void set_param_field(struct Struct* s) {
  s->inner.g = 3E+9;
}

char return_soa(struct SOA* param, int i, int j, int k) {
  return param[i].s[j].pad[k];
}

void set_union(union Union* u) {
  u->l = 45;
  u->i = 12;
  u->c = 'a';
}

long anon_union(int a, int b, short c) {
  union {
    long l;
    int i;
    char c;
  } local;
  if(a)
    local.i = c;
  else if(b)
    local.l = c;
  else
    local.c = 'a';

  return local.l;
}

struct Struct2 {
  int a;
  struct Struct* s;
};

long array_deref(struct Struct2* ptr, int m, int n) {
  return ptr[m].s[0].b[0];
}
