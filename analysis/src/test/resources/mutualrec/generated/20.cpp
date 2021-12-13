
// a.u -> h.w -> a.u
// h.w -> a.u -> h.w


class A {
  public:
    virtual void u(){w();};
    virtual void w(){};
};

class O : public A{
  public:
    virtual void v(){};
    virtual void u(){};
};

class H : public A{
  public:
    virtual void o(){};
    virtual void w(){u();};
};
