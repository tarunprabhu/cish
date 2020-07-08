int f();
void g(int);
void h(void*);

void br_if(int a, int b, int c) {
  unsigned int d = a;
  if(b < 0)
    h(&d);
}

void br_if_else(int a) {
  if(a)
    g(34);
  else
    g(36);
}

void br_if_else_if(int a, int b) {
  if(a)
    g(34);
  else if(b)
    g(324);
  else
    g(93);
}

void brs(int a, int b, int c, int d) {
  if(a)
    g(1);
  if(b)
    g(2);
  if(c)
    g(3);
  if(d)
    g(4);
}

int return_br_if(int a, int b, int c) {
  int d = a;
  if(b > 11)
    d += c;
  return d;
}

void switch_wtf(int a) {
  switch(a) {
    g(a);
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
    g(1);
    break;
  case 6:
    break;
  case 5:
    break;
  case 4:
    g(2);
    break;
  default:
    g(4);
    break;
  }
}

int switch_no_fallthrough(int a, int b, int c) {
  int d;
  switch(a) {
  case 1:
    d = b + f();
    break;
  case 10:
    break;
  case 2:
    d = c + f();
    break;
  case 3:
    d = a + f();
    break;
  default:
    d = f();
    break;
  }
  return d;
}

void switch_loops_fallthrough(int a, int b, int c) {
  switch(a) {
  case 1:
    for(int i = 0; i < b; i++)
      g(i);
  case 2:
    for(int i = 0; i < b; i += c)
      g(i + a);
    break;
  case 3:
    while(f())
      g(f());
    break;
  }
}

void switch_endless(int a, int b) {
  switch(a) {
  case 1:
    while(1);
  case 2:
    for(int i = 0; i < b; i++)
      g(i + b);
    break;
  }
}

int switch_fallthrough(int a, int b, int c) {
  int d = a + b + c;
  switch(a) {
  case 1:
    d += f();
  case 2:
    d = c + f();
    break;
  case 3:
    d = a + f();
    break;
  default:
    d = f();
    break;
  }
  return d;
}

// From "Advanced Compiler Design and Implementation: Steven Muchnick".
int muchnick_book(int a, int b) {
  if(a) {
    if(b) {
      g(1);
    }
    g(2);
  } else {
    do {
      if(f()) {
        g(3);
      } else {
        g(4);
      }
      g(5);
    } while((f()));
    g(6);
  }
  return f();
}

// From "No More Gotos: Decompilation Using Pattern-Independent Control-Flow
// Structuring and Semantics-Preserving Transformations. Yakdan et. al"
void no_more_gotos(int a, int b1, int b2) {
  if(a) {
    do {
      while(f())
        g(0xA1);
      if(f()) {
        g(0xA2);
        break;
      }
      g(0xA3);
    } while(f());
  } else {
    if(!b1)
      g(0xA4);
    if(b1 && b2)
      g(0xA6);
    else
      g(0xA5);
    g(0xA7);
    while(f())
      g(0xA8);
  }
  g(0xA9);
}
