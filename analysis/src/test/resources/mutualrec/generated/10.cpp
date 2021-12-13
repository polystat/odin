
// l.h -> n.u -> l.h
// n.u -> l.h -> n.u


class L {
  public:
    virtual void u(){};
    virtual void h(){u();};
    virtual void s(){};
};

class N : public L{
  public:
    virtual void l(){s();};
    virtual void u(){h();};
};

class B : public L{
  public:
    virtual void y(){u();};
    virtual void k(){};
    virtual void s(){};
};
