
// d.p -> j.g -> d.p
// j.q -> j.g -> d.p -> j.g
// j.g -> d.p -> j.g


class D {
  public:
    virtual void i(){g();};
    virtual void p(){g();};
    virtual void g(){};
};

class V {
  public:
    virtual void v(){j();};
    virtual void m(){};
    virtual void j(){m();};
};

class J : public D{
  public:
    virtual void q(){g();};
    virtual void i(){};
    virtual void g(){p();};
};
