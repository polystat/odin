// h.l -> b.p -> h.l
// h.e -> b.p -> h.l -> b.p
// b.p -> h.l -> b.p

class H {
  public:
    virtual void l(){p();};
    virtual void p(){};
    virtual void e(){p();};
};

class B : public H{
  public:
    virtual void m(){};
    virtual void p(){l();};
};

class L {
  public:
    virtual void f(){r();};
    virtual void r(){};
};

