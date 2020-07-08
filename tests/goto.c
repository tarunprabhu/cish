int f();
void g(int);

int goto_end() {
  if(f())
    goto exit;

 exit:
  return f();
}

int goto_multiple(int a, int b) {
  int n = a % b + f();
  int i;

  if(a) {
    if(a % b)
      goto exit;
    n -= a;
  }

  for(i = 0; i < n; i++)
    g(i);

 exit:
  return f();
}
