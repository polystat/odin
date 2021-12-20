// m.y -> e.t -> e.z -> m.y
// e.z -> m.y -> e.t -> e.z
// e.t -> e.z -> m.y -> e.t

class M {
  public:
    virtual void y(){t();};
    virtual void g(){};
    virtual void t(){g();};
};

class E : public M{
  public:
    virtual void z(){y();};
    virtual void t(){z();};
    virtual void g(){};
};

class A {
  public:
    virtual void s(){};

    class D {
    public:
      virtual void i(){m();};
      virtual void a(){m();};
      virtual void d(){};
      virtual void m(){d();};
    };
};

