#include <cstdlib>

void empty() {

}

int arg_ref(int& n) {
  return n + rand();
}

int& return_ref(int& n) {
  return n;
}

int return_ref_deref(int& n) {
  return n;
}

int* return_ref_addr(int& n) {
  return &n;
}

void store_ref(int& n) {
  n = 23;
}
