// o.u -> r.q -> o.i -> r.q
// r.q -> o.i -> r.q
// o.i -> r.q -> o.i

class R {
  public:
    virtual void q(){i();};
    virtual void j(){};
    virtual void u(){};
    virtual void i(){};
};

class V {
  public:
    virtual void s(){};
};

class O : public R{
  public:
    virtual void d(){};
    virtual void u(){q();};
    virtual void i(){q();};
};

