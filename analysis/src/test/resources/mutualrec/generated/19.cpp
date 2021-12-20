// l.s -> l.k -> z.y -> l.k
// l.k -> z.y -> l.k
// z.y -> l.k -> z.y
// l.r -> l.k -> z.y -> l.k

class L {
  public:
    virtual void s(){k();};
    virtual void r(){k();};
    virtual void y(){};
    virtual void k(){y();};
};

class Z : public L{
  public:
    virtual void a(){};
    virtual void y(){k();};
};

