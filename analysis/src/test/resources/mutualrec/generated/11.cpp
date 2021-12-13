
// o.x -> z.q -> o.x
// z.q -> o.x -> z.q


class O {
  public:
    virtual void q(){};
    virtual void c(){};
    virtual void y(){x();};
    virtual void x(){q();};
};

class Z : public O{
  public:
    virtual void c(){};
    virtual void g(){};
    virtual void y(){};
    virtual void q(){x();};
};

class Q : public O{
  public:
    virtual void s(){};
    virtual void q(){};
};
