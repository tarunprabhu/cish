int return_arg_deref(int *n) {
    return *n;
}

unsigned long return_alloca(unsigned long* init) {
  unsigned long local = *init;
  return local;
}

int return_deref_darray_param_var(const int* arr, long idx) {
  return arr[idx];
}

long double return_deref_darray_param_const(const long double* arr) {
  return arr[1];
}

int return_deref_sarray_param_const_1(const int arr[3]) {
  return arr[2];
}

long return_deref_sarray_param_const_2(const long arr[4][5]) {
  return arr[3][2];
}

double return_deref_static_array_1(unsigned idx) {
  double arr[3];
  return arr[idx];
}

float return_deref_static_array_2(long idx1, long idx2) {
  float arr[3][4];
  return arr[idx1][idx2];
}
