#include <stdlib.h>

void br_if(int a, int b, int c) {
  unsigned int d = a;
  if(b < 0)
    rand_r(&d);
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

void brs(int a, int b, int c, int d) {
  if(a)
    srand(1);
  if(b)
    srand(2);
  if(c)
    srand(3);
  if(d)
    srand(4);
}

int return_br_if(int a, int b, int c) {
  int d = a;
  if(b > 11)
    d += c;
  return d;
}

void switch_wtf(int a) {
  switch(a) {
    srand(a);
  }
}

void switch_empty(int a) {
  switch(a) {
    ;
  }
}

void switch_empty_cases(int a) {
  switch(a) {
  case 3: break;
  case 2: break;
  case 1: break;
  case 0: break;
  }
}

void switch_some_empty_cases(int a) {
  switch(a) {
  case 7:
    srand(1);
    break;
  case 6:
    break;
  case 5:
    break;
  case 4:
    srand(2);
    break;
  default:
    srand(4);
    break;
  }
}

int switch_no_fallthrough(int a, int b, int c) {
  int d;
  switch(a) {
  case 1:
    d = b + rand();
    break;
  case 10:
    break;
  case 2:
    d = c + rand();
    break;
  case 3:
    d = a + rand();
    break;
  default:
    d = rand();
    break;
  }
  return d;
}

void switch_loops_fallthrough(int a, int b, int c) {
  switch(a) {
  case 1:
    for(int i = 0; i < b; i++)
      srand(i);
  case 2:
    for(int i = 0; i < b; i += c)
      srand(i + a);
    break;
  case 3:
    while(rand())
      srand(rand());
    break;
  }
}

void switch_endless(int a, int b) {
  switch(a) {
  case 1:
    while(1);
  case 2:
    for(int i = 0; i < b; i++)
      srand(i + b);
    break;
  }
}

int switch_fallthrough(int a, int b, int c) {
  int d = a + b + c;
  switch(a) {
  case 1:
    d += rand();
  case 2:
    d = c + rand();
    break;
  case 3:
    d = a + rand();
    break;
  default:
    d = rand();
    break;
  }
  return d;
}

// From "Advanced Compiler Design and Implementation: Steven Muchnick".
int muchnick_book(int a, int b) {
  if(a) {
    if(b) {
      srand(1);
    }
    srand(2);
  } else {
    do {
      if(rand()) {
        srand(3);
      } else {
        srand(4);
      }
      srand(5);
    } while((rand()));
    srand(6);
  }
  return rand();
}

// From "No More Gotos: Decompilation Using Pattern-Independent Control-Flow
// Structuring and Semantics-Preserving Transformations. Yakdan et. al"
void no_more_gotos(int a, int b1, int b2) {
  if(a) {
    do {
      while(rand())
        srand(0xA1);
      if(rand()) {
        srand(0xA2);
        break;
      }
      srand(0xA3);
    } while(rand());
  } else {
    if(!b1)
      srand(0xA4);
    if(b1 && b2)
      srand(0xA6);
    else
      srand(0xA5);
    srand(0xA7);
    while(rand())
      srand(0xA8);
  }
  srand(0xA9);
}
