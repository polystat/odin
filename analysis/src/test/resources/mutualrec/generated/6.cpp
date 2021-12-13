
// z.x -> p.a -> z.x
// p.r -> p.a -> z.x -> p.a
// p.t -> z.x -> p.a -> z.x
// p.a -> z.x -> p.a


class Z {
  public:
    virtual void x(){a();};
    virtual void a(){};
};

class P : public Z{
  public:
    virtual void r(){a();};
    virtual void t(){x();};
    virtual void a(){x();};
};

class X {
  public:
    virtual void b(){d();};
    virtual void w(){};
    virtual void d(){};
};
