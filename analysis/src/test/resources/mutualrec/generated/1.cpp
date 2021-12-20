// c.x.f -> f.q -> c.x.f
// f.q -> c.x.f -> f.q

class C {
  public:
    virtual void i(){};
    virtual void q(){m();};
    virtual void j(){};
    virtual void m(){j();};

    class X : public C{
    public:
      virtual void f(){q();};
      virtual void i(){j();};
    };
};

class F : public C::X{
  public:
    virtual void j(){};
    virtual void h(){j();};
    virtual void q(){f();};
    virtual void i(){};
    virtual void s(){m();};
};

