#include <stdbool.h>

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

int return_neg(int a) {
  return -a;
}

int return_add_assign(int a, int b) {
  a += b;
  return a;
}

int return_sub_assign(int a, int b) {
  a -= b;
  return a;
}

int return_mul_assign(int a, int b) {
  a *= b;
  return a;
}

int return_div_assign(int a, int b) {
  a /= b;
  return a;
}

int return_rem_assign(int a, int b) {
  a %= b;
  return a;
}

int return_shl_assign(int a, int b) {
  a <<= b;
  return a;
}

int return_shr_assign(int a, int b) {
  a >>= b;
  return a;
}

int return_and_assign(int a, int b) {
  a &= b;
  return a;
}

int return_or_assign(int a, int b) {
  a |= b;
  return a;
}

int return_xor_assign(int a, int b) {
  a ^= b;
  return a;
}
