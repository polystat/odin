
// l.m -> w.p -> w.f -> l.m
// w.f -> l.m -> w.p -> w.f
// w.p -> w.f -> l.m -> w.p
// w.y -> l.m -> w.p -> w.f -> l.m


class L {
  public:
    virtual void p(){};
    virtual void m(){p();};
    virtual void y(){};
};

class O : public L{
  public:
    virtual void h(){};
    virtual void m(){};
    virtual void y(){};
    virtual void k(){y();};
};

class W : public L{
  public:
    virtual void f(){m();};
    virtual void p(){f();};
    virtual void y(){m();};
};
