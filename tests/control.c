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

int switch_no_fallthrough(int a, int b, int c) {
  int d;
  switch(a) {
  case 1:
    d = b + rand();
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

void infinite_while_empty() {
  while(1);
}

void infinite_while_body(int a) {
  while(1) {
    srand(a);
  }
}

void infinite_while_body_br(int a, int b) {
  while(1) {
    if(a)
      srand(1);
    if(b)
      srand(3);
    else
      srand(4);
  }
}

void for_loop_infinite(int a) {
  for(long i; ; i++)
    if(i > 0)
      srand(a + i);
}

void for_loop(int a) {
  for(int i = 0; i < a; i++)
    srand(i);
}

void for_loop_2(int a, int b) {
  for(int i = 0; i < a; i++)
    if(i == rand())
      srand(a);
    else
      srand(b);
}

void for_loop_nest_perfect(int a, int b, int c) {
  for(int i = 0; i < a; i++)
    for(int j = 0; j < b; j++)
      for(int k = 0; k < c; k++)
        srand(i * j * k);
}

void for_loop_nest_imperfect(int a, int b, int c) {
  for(int i = 0; i < a; i++) {
    for(int j = 0; j < b; j++)
      srand(i * j);
    for(int k = 0; k < c; k++)
      srand(k * i);
  }
}

void while_loop(int a) {
  while(1) {
    srand(1);
    if(a == rand())
      break;
    srand(2);
  }
}

void for_loop_continue(int a) {
  for(int i = 0; i < a; i++) {
    srand(a + 1);
    if(i == rand())
      continue;
    else
      srand(i + 1);
    srand(a + 2);
  }
}

void while_loop_break_continue(int a) {
  srand(6);
  while(1) {
    srand(1);
    if(a == rand()) {
      srand(3);
      continue;
    } else if(rand() > 100) {
      srand(4);
      break;
    }
    srand(2);
  }
  srand(5);
}

void for_loop_break_continue(int a) {
  srand(6);
  for(int i = 0; i < a; i++) {
    srand(1);
    /* if(a == rand()) { */
    /*   srand(3); */
    /*   continue; */
    /* } else */ if(rand() > 100) {
      srand(4);
      break;
    }
    srand(2);
  }
  srand(5);
}

void for_loop_multiple_contine(int a) {
  srand(6);
  for(int i = 0; i < a;) {
    srand(1);
    if(i == rand()) {
      i++;
      continue;
    } else if(a == rand()) {
      srand(3);
      continue;
    } else if((a + i) == rand()) {
      srand(4);
      continue;
    }
    srand(2);
  }
  srand(5);
}

void for_loop_multiple_break(int a) {
  srand(6);
  for(int i = 0; i < a; ) {
    srand(1);
    if(i == rand()) {
      srand(3);
      break;
    } else if(a == rand()) {
      srand(4);
      break;
    } else if((a + i) == rand()) {
      srand(7);
      break;
    }
    srand(2);
  }
  srand(5);
}

void loop_unconditional_break(int a) {
  srand(1);
  for(int i = 0; i < a; i++) {
    srand(2);
    break;
  }
  srand(3);
}

void for_loop_diff_init(int a, int b, int c, int d) {
  int i = a;
  if(rand())
    i = rand() + b;
  for(; i < c; i += d)
    srand(i);
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
