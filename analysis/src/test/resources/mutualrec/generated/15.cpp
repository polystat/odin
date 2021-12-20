// p.s -> p.x -> p.l -> k.u -> p.s
// p.x -> p.l -> k.u -> p.s -> p.x
// k.u -> p.s -> p.x -> p.l -> k.u
// p.l -> k.u -> p.s -> p.x -> p.l

class K {
  public:
    virtual void x(){u();};
    virtual void l(){};
    virtual void s(){};
    virtual void u(){s();};
};

class T {
  public:
    virtual void z(){};
    virtual void r(){z();};

    class Z {
    public:
      virtual void g(){};
      virtual void f(){g();};
    };

    class G {
    public:
      virtual void g(){m();};
      virtual void m(){};
      virtual void f(){};
    };

    class U {
    public:
      virtual void i(){e();};
      virtual void a(){i();};
      virtual void e(){};
    };
};

class P : public K{
  public:
    virtual void s(){x();};
    virtual void x(){l();};
    virtual void z(){};
    virtual void l(){u();};
};

