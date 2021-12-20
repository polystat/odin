// w.u.w -> n.p -> w.u.w
// n.p -> w.u.w -> n.p

class W {
  public:
    virtual void w(){i();};
    virtual void c(){};
    virtual void i(){};

    class U {
    public:
      virtual void w(){p();};
      virtual void p(){};
    };
};

class N : public W::U{
  public:
    virtual void i(){};
    virtual void p(){w();};
};

