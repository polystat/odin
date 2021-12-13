
// d.j -> p.n -> d.j
// p.g -> d.j -> p.n -> d.j
// p.n -> d.j -> p.n
// p.d -> p.g -> d.j -> p.n -> d.j


class D {
  public:
    virtual void j(){n();};
    virtual void d(){};
    virtual void n(){};
};

class S : public D{
  public:
    virtual void k(){n();};
    virtual void j(){d();};
    virtual void d(){k();};
    virtual void n(){};
};

class P : public D{
  public:
    virtual void g(){j();};
    virtual void n(){j();};
    virtual void d(){g();};
};
