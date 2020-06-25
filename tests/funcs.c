int f(int);
void g(int);
int h(void*);

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
  g(389);
}

int return_funcall_with_alloca(unsigned int init) {
  unsigned int seed = init;
  return h(&seed);
}

void funcall_stmts() {
  g(1);
  g(2);
  g(3);
  g(4);
}

void mutate(int *i) {
  *i = 3;
}

int indirect_mutate(int a) {
  int r;
  mutate(&a);
  r = a + 1;
  return r;
}
