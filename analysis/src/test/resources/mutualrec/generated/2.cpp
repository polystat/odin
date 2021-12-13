
// y.n -> g.j -> g.c -> y.n
// g.s -> g.c -> y.n -> g.j -> g.c
// g.j -> g.c -> y.n -> g.j
// g.c -> y.n -> g.j -> g.c


class Y {
  public:
    virtual void n(){j();};
    virtual void c(){};
    virtual void j(){};
};

class G : public Y{
  public:
    virtual void s(){c();};
    virtual void j(){c();};
    virtual void c(){n();};
};

class D : public Y{
  public:
    virtual void a(){c();};
    virtual void n(){};
    virtual void j(){a();};
};
