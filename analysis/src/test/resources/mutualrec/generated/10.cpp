// f.o -> t.e -> f.o
// t.e -> f.o -> t.e

class F {
  public:
    virtual void m(){};
    virtual void o(){e();};
    virtual void e(){};
    virtual void j(){};

    class E {
    public:
      virtual void s(){x();};
      virtual void x(){};
      virtual void t(){};
      virtual void w(){};
    };
};

class T : public F{
  public:
    virtual void v(){};
    virtual void j(){};
    virtual void e(){o();};

    class E {
    public:
      virtual void s(){x();};
      virtual void x(){};
      virtual void t(){};
      virtual void w(){};
    };
};

