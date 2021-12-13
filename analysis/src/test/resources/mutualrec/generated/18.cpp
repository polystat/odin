
// c.m -> x.d -> c.y -> c.f -> x.d
// c.y -> c.f -> x.d -> c.y
// x.d -> c.y -> c.f -> x.d
// c.f -> x.d -> c.y -> c.f


class X {
  public:
    virtual void f(){y();};
    virtual void d(){y();};
    virtual void y(){};
};

class U : public X{
  public:
    virtual void n(){f();};
    virtual void d(){c();};
    virtual void c(){};
};

class C : public X{
  public:
    virtual void m(){d();};
    virtual void o(){};
    virtual void y(){f();};
    virtual void f(){d();};
};
