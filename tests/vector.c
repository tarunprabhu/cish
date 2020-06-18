void vecadd(const double* __restrict__ a,
            const double* __restrict__ b,
            double* __restrict__ y,
            unsigned long n) {
  for(unsigned long i = 0; i < n; i++)
    y[i] = a[i] * b[i];
}

int* matmul(const int* __restrict__ p_a,
            const int* __restrict__ p_b,
            int* __restrict__ p_y,
            int n) {
  const int (*a)[n][n] = (const int(*)[n][n])p_a;
  const int (*b)[n][n] = (const int(*)[n][n])p_b;
  int (*y)[n][n] = (int(*)[n][n])p_y;
  for(int i = 0; i < n; i++) {
    for(int j = 0; j < n; j++) {
      (*y)[i][j] = 0;
      for(int k = 0; k < n; k++)
        (*y)[i][j] += (*a)[i][k] * (*b)[k][j];
    }
  }
  return p_y;
}
