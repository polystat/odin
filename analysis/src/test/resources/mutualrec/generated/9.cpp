// m.m -> b.f -> b.i -> m.m
// b.i -> m.m -> b.f -> b.i
// b.f -> b.i -> m.m -> b.f

class M {
  public:
    virtual void m(){f();};
    virtual void f(){};
};

class B : public M{
  public:
    virtual void i(){m();};
    virtual void f(){i();};
};

