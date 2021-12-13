
// b.g -> k.s -> b.g
// k.s -> b.g -> k.s
// b.h -> k.s -> b.g -> k.s
// b.g -> k.s -> b.g
// k.s -> b.g -> k.s


class K {
  public:
    virtual void u(){};
    virtual void g(){};
    virtual void s(){g();};
    virtual void c(){};
};

class B : public K{
  public:
    virtual void l(){};
    virtual void g(){s();};
    virtual void c(){u();};
    virtual void h(){s();};
};

class I : public B{
  public:
    virtual void x(){};
    virtual void h(){c();};
};
