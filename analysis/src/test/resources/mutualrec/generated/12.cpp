
// w.z -> c.i -> w.z
// c.i -> w.z -> c.i
// c.r -> w.z -> c.i -> w.z
// o.r -> o.i -> w.z -> o.i
// w.z -> o.i -> w.z
// o.i -> w.z -> o.i


class W {
  public:
    virtual void i(){};
    virtual void z(){i();};
    virtual void r(){};
};

class C : public W{
  public:
    virtual void g(){};
    virtual void i(){z();};
    virtual void r(){z();};
};

class O : public W{
  public:
    virtual void r(){i();};
    virtual void a(){};
    virtual void e(){};
    virtual void i(){z();};
};
