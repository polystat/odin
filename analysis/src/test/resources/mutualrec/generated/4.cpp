
// z.x -> h.a -> z.x
// h.a -> z.x -> h.a


class H {
  public:
    virtual void j(){};
    virtual void a(){x();};
    virtual void i(){j();};
    virtual void x(){};
};

class E : public H{
  public:
    virtual void d(){};
    virtual void j(){};
    virtual void a(){};
    virtual void g(){a();};
    virtual void i(){};
    virtual void x(){i();};
};

class Z : public H{
  public:
    virtual void x(){a();};
    virtual void c(){i();};
    virtual void i(){};
    virtual void v(){};
};
