void vecadd(const double* __restrict__ a,
            const double* __restrict__ b,
            double* __restrict__ y,
            unsigned long n) {
  for(unsigned long i = 0; i < n; i++)
    y[i] = a[i] * b[i];
}
