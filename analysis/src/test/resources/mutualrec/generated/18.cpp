// g.l -> g.u -> g.j -> g.y -> l.n -> g.y
// l.n -> g.y -> l.n
// g.u -> g.j -> g.y -> l.n -> g.y
// g.y -> l.n -> g.y
// g.j -> g.y -> l.n -> g.y
// n.n -> g.y -> n.n
// g.y -> n.n -> g.y

class L {
  public:
    virtual void n(){y();};
    virtual void j(){y();};
    virtual void y(){};
};

class G : public L{
  public:
    virtual void l(){u();};
    virtual void u(){j();};
    virtual void y(){n();};
    virtual void j(){y();};
};

class N : public G{
  public:
    virtual void j(){l();};
    virtual void q(){u();};
    virtual void l(){};
    virtual void n(){y();};
};

