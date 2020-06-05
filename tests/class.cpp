class Class {
private:
  int m;

public:
  Class(int m) : m(m) {}
  virtual int get() {
    return m;
  }
} cls(21398);

class Base {
protected:
  int p;
public:
  Base(int p) : p(p) {}
  virtual ~Base() = default;
  virtual int f() = 0;
  virtual int g() = 0;
};

class Derived1 : public Base {
protected:
  int m;

public:
  Derived1(int m) : Base(12), m(m), m1(m*m) {}
  virtual ~Derived1() = default;

  virtual int f() override {
    return m + 23;
  }
  virtual int g() override {
    return 21 + p + m1;
  }

public:
  int m1;
};

class Derived2 : public Base {
protected:
  int n;

public:
  Derived2(int n) : Base(16), n(n) {}
  virtual ~Derived2() = default;

  virtual int f() override {
    return 11 + p;
  }
  virtual int g() override {
    return n + 99;
  }
};

class Multiple : public Class, public Derived1 {
protected:
  int x;

public:
  Multiple(int m, int x) : Class(m), Derived1(m + x), x(x) {}
  virtual int get(int a) override {
    if(a)
      return Class::get() + g();
    else
      return Class::get() + f();
  }
} m(102, 121);

int call_virtual(int a, int b, int c, int d) {
  Derived1 d1(b);
  Derived2 d2(c);
  Base* p = nullptr;

  if(a)
    p = &d1;
  else
    p = &d2;

  if(d)
    return p->f();
  else
    return p->g();
}

Derived1 ret_by_value(Derived1* d) {
  return *d;
}
