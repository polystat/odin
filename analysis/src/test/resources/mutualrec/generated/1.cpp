
// x.y -> p.c -> x.y
// p.c -> x.y -> p.c


class I {
  public:
    virtual void b(){};
    virtual void y(){};
    virtual void k(){};
};

class X {
  public:
    virtual void y(){c();};
    virtual void c(){};
};

class P : public X{
  public:
    virtual void w(){};
    virtual void o(){};
    virtual void c(){y();};
};
