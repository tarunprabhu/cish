void g(int);
int f();

void infinite_while_empty() {
  while(1);
}

void infinite_while_body(int a) {
  while(1) {
    g(a);
  }
}

void infinite_while_body_br(int a, int b) {
  while(1) {
    if(a)
      g(1);
    if(b)
      g(3);
    else
      g(4);
  }
}

void for_loop_infinite(int a) {
  for(long i; ; i++)
    if(i > 0)
      g(a + i);
}

void for_loop(int a) {
  for(int i = 0; i < a; i++)
    g(i);
}

void for_loop_2(int a, int b) {
  for(int i = 0; i < a; i++)
    if(i == f())
      g(a);
    else
      g(b);
}

void for_loop_nest_perfect(int a, int b, int c) {
  for(int i = 0; i < a; i++)
    for(int j = 0; j < b; j++)
      for(int k = 0; k < c; k++)
        g(i * j * k);
}

void for_loop_nest_imperfect(int a, int b, int c) {
  for(int i = 0; i < a; i++) {
    for(int j = 0; j < b; j++)
      g(i * j);
    for(int k = 0; k < c; k++)
      g(k * i);
  }
}

void while_loop(int a) {
  while(1) {
    g(1);
    if(a == f())
      break;
    g(2);
  }
}

void for_loop_continue(int a) {
  for(int i = 0; i < a; i++) {
    g(a + 1);
    if(i == f())
      continue;
    else
      g(i + 1);
    g(a + 2);
  }
}

void while_loop_break_continue(int a) {
  g(6);
  while(1) {
    g(1);
    if(a == f()) {
      g(3);
      continue;
    } else if(f() > 100) {
      g(4);
      break;
    }
    g(2);
  }
  g(5);
}

void for_loop_break_continue(int a) {
  g(6);
  for(int i = 0; i < a; i++) {
    g(1);
    while(1)
      if(i == f())
        break;
    if(a == f()) {
      g(3);
      continue;
    } else if(f() > 100) {
      break;
    }
    g(2);
    g(4);
  }
  g(5);
}

void for_loop_multiple_contine(int a) {
  g(6);
  for(int i = 0; i < a;) {
    g(1);
    if(i == f()) {
      i++;
      continue;
    } else if(a == f()) {
      g(3);
      continue;
    } else if((a + i) == f()) {
      g(4);
      continue;
    }
    g(2);
  }
  g(5);
}


void while_loop_multiple_break_empty(int a) {
  g(6);
  int i = 0;
  while(1) {
    g(1);
    if(i == f()) {
      break;
    } else if(a == f()) {
      break;
    } else if((a + i) == f()) {
      break;
    }
    i++;
    g(2);
  }
  g(5);
}

void for_loop_multiple_break_empty(int a) {
  g(6);
  for(int i = 0; i < a; i++) {
    g(1);
    if(i == f()) {
      break;
    } else if(a == f()) {
      break;
    } else if((a + i) == f()) {
      break;
    }
    g(2);
  }
  g(5);
}

void for_loop_multiple_break(int a) {
  g(6);
  for(int i = 0; i < a; i++) {
    g(1);
    if(i == f()) {
      g(3);
      break;
    } else if(a == f()) {
      g(4);
      break;
    } else if((a + i) == f()) {
      g(7);
      break;
    }
    g(2);
  }
  g(5);
}

void loop_unconditional_break(int a) {
  g(1);
  for(int i = 0; i < a; i++) {
    g(2);
    break;
  }
  g(3);
}

void for_loop_diff_init(int a, int b, int c, int d) {
  int i = a;
  if(f())
    i = f() + b;
  for(; i < c; i += d)
    g(i);
}
