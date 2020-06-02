#include <stdboolh>

int return_plus(int a, int b) {
  return a + b;
}

float return_minus(float a, float b) {
  return a - b;
}

long return_times(long a, long b) {
  return a * b;
}

double return_divide(double a, double b) {
  return a / b;
}

short return_modulo(short a, short b) {
  return a % b;
}

bool return_logical_and(char a, char b) {
  return a && b;
}

bool return_logical_or(short a, short b) {
  return a || b;
}

bool return_logical_not(bool b) {
  return !b;
}

int return_bitwise_and(int a, int b) {
  return a & b;
}

int return_bitwise_or(int a, int b) {
  return a | b;
}

int return_bitwise_xor(int a, int b) {
  return a ^ b;
}

unsigned long return_bitwise_shift_left(unsigned long a, int b) {
  return a << b;
}

unsigned long return_bitwise_shift_right(unsigned long a, int b) {
  return a >> b;
}

bool return_eq(float a, float b) {
  return a == b;
}

bool return_ne(double a, double b) {
  return a != b;
}

bool return_lt(long double a, long double b) {
  return a < b;
}

bool return_le(char a, char b) {
  return a <= b;
}

bool return_gt(short a, short b) {
  return a > b;
}

bool return_ge(int a, int b) {
  return a >= b;
}
