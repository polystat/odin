
// x.l -> e.a -> x.l
// e.n -> x.l -> e.a -> x.l
// e.a -> x.l -> e.a
// e.a -> x.l -> e.a
// c.n -> x.l -> e.a -> x.l
// x.l -> e.a -> x.l


class X {
  public:
    virtual void a(){};
    virtual void l(){a();};
};

class E : public X{
  public:
    virtual void u(){};
    virtual void n(){l();};
    virtual void a(){l();};
};

class C : public E{
  public:
    virtual void n(){l();};
    virtual void v(){};
};
