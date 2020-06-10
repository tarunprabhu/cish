#include <stdbool.h>

// Scalars
bool bt = true;
bool bf = false;
char c = 'a';
unsigned char uc = 188;
short s = -11;
unsigned short us = 842;
int i = -42;
unsigned int ui = 938298U;
long l = -290120129;
unsigned long ul = 0xdeadbeef100UL;
float f = 2.1e+8;
double g = -194.29E-19;
long double lg = 110.209E+412L;

// Pointers
void* const ptr = 0;
const void* ptr_c = 0;

// Strings/char arrays
char cstr[10] = "This";
char bstr[4] = {48, 49, 50};
const char* str = "That";

// Other arrays
int arr[3] = {1, 2, 3};
int arr2[2][3] = {{1, 2, 3}, {4, 5, 6}};
int arr3[][2][3] = {{{1, 2, 3}, {4, 5, 6}},
                    {{4, 5, 6}, {7, 8, 9}},
                    {{10, 11, 12}, {13, 14, 15}},
                    {{16, 17, 18}, {19, 20, 21}}};

// Structs
struct Outer {
  int i;
  char j;
  struct Inner {
    float l;
    double m;
  } k;
} obj = {10, 'a', {3.4, 6.2}};

struct Pointers {
  int* i;
  const int* ci;
  int const* ic;
  const int* const* cic;
} ptrs = {0, 0, 0, 0};
