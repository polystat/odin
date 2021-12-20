// v.f -> d.u -> v.f
// d.u -> v.f -> d.u

class V {
  public:
    virtual void u(){};
    virtual void f(){u();};
};

class D : public V{
  public:
    virtual void n(){};
    virtual void u(){f();};
};

