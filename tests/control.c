#include <stdlib.h>

void br_if(int a, int b, int c) {
  unsigned int d = a;
  if(b < 0)
    rand_r(d);
}

void br_if_else(int a) {
  if(a)
    srand(34);
  else
    srand(36);
}

void br_if_else_if(int a, int b) {
  if(a)
    srand(34);
  else if(b)
    srand(324);
  else
    srand(93);
}

int return_br_if(int a, int b, int c) {
  int d = a;
  if(b > 11)
    d += c;
  return d;
}

void for_loop(int a) {
  for(int i = 0; i < a; i++)
    srand(i);
}

void while_loop(int a) {
  while(1) {
    if(a == rand())
      break;
    srand(rand());
  }
}

void for_loop_continue(int a) {
  for(int i = 0; i < a; i++) {
    if(a == rand())
      continue;
    srand(i);
  }
}

void while_loop_break_continue(int a) {
  while(1) {
    if(a == rand())
      continue;
    else if(rand() > 100)
      break;
    srand(rand());
  }
}

void for_loop_break_continue(int a) {
  for(int i = 0; i < a; i++) {
    if(a == rand())
      continue;
    else if(i == rand())
      break;
    srand(i);
  }
}
