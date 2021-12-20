// t.r -> r.o -> t.y -> r.o
// r.e -> t.y -> r.o -> t.y
// t.y -> r.o -> t.y
// r.o -> t.y -> r.o

class B {
  public:
    virtual void d(){o();};
    virtual void o(){};
};

class R : public B{
  public:
    virtual void y(){};
    virtual void e(){y();};
    virtual void d(){};
    virtual void o(){y();};
};

class T : public R{
  public:
    virtual void w(){};
    virtual void r(){o();};
    virtual void d(){};
    virtual void y(){o();};
};

