#include <stdlib.h>

void empty() {
    ;
}

void empty_scalar_arg(int n) {
    ;
}

void empty_pointer_arg(int* n) {
    ;
}

int return_0() {
    return 0;
}

int return_arg(int n) {
    return n;
}

void funcall() {
  srand(389);
}

int return_funcall_with_alloca(unsigned int init) {
  unsigned int seed = init;
  return rand_r(&seed);
}

void funcall_stmts() {
  srand(1);
  srand(2);
  srand(3);
  srand(4);
}
