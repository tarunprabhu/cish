#include <stdlib.h>

int g(int);

int garr[3][4][2] = {{{1, 2}, {3, 4}, {5, 6}, {7, 8}},
                     {{11, 12}, {13, 14}, {15, 16}, {17, 18}},
                     {{21, 22}, {23, 24}, {25, 26}, {27, 28}}};

int array_get(int i, int j, int k) {
  return garr[i][j][k];
}

int* array_subarray(int i, int j) {
  return garr[i][j];
}

int* array_subarray2(int i, int j, int k) {
  int* ptr;
  if(k)
    return &garr[i][j][k];
  else
    return (int*)garr[j][k];
}

int array_indirect(int i, int j, int k) {
  int* ptr;
  if(k)
    ptr = garr[i][j];
  else
    ptr = garr[j][i];
  return ptr[k];
}

int dyn_array(int d1, int n) {
  int* arr = (int*)malloc(sizeof(int) * d1);
  for(int i = 0; i < d1; i++)
    arr[i] = g(i);
  return arr[n];
}

int dyn_array_2(int d1, int d2, int m, int n) {
  int** arr = (int**)malloc(sizeof(int*) * d1);
  for(int i = 0; i < d1; i++)
    arr[i] = (int*)malloc(sizeof(int) * d2);

  for(int i = 0; i < d1; i++)
    for(int j = 0; j < d2; j++)
      arr[i][j] = g(i + j);
  return arr[m][n];
}
