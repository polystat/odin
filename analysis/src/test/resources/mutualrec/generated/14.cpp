// a.v -> r.h -> a.v
// r.h -> a.v -> r.h

class A {
  public:
    virtual void v(){h();};
    virtual void y(){};
    virtual void h(){y();};
};

class R : public A{
  public:
    virtual void x(){};
    virtual void h(){v();};
    virtual void y(){};
};

class M {
  public:
    virtual void o(){x();};
    virtual void x(){};
};

