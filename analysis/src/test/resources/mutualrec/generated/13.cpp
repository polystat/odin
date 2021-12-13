
// d.u -> d.k -> m.j -> m.s -> d.u
// d.k -> m.j -> m.s -> d.u -> d.k
// m.j -> m.s -> d.u -> d.k -> m.j
// m.s -> d.u -> d.k -> m.j -> m.s


class M {
  public:
    virtual void j(){s();};
    virtual void s(){u();};
    virtual void l(){};
    virtual void u(){};
};

class D : public M{
  public:
    virtual void u(){k();};
    virtual void k(){j();};
    virtual void l(){};
};

class F {
  public:
    virtual void w(){j();};
    virtual void c(){};
    virtual void j(){};
};
