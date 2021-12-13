
// z.g -> x.b -> z.g
// x.w -> z.g -> x.b -> z.g
// x.b -> z.g -> x.b


class X {
  public:
    virtual void w(){g();};
    virtual void b(){g();};
    virtual void g(){};
};

class Y {
  public:
    virtual void w(){};
};

class Z : public X{
  public:
    virtual void g(){b();};
    virtual void x(){};
    virtual void t(){};
};
