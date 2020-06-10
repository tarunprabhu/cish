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
