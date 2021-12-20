// h.p.i.y -> v.h -> h.p.i.y
// v.h -> h.p.i.y -> v.h

class H {
  public:
    virtual void p(){};
    virtual void k(){};
    virtual void f(){};
    virtual void w(){};

    class P {
    public:
      virtual void o(){};
      virtual void h(){};

      class I {
      public:
        virtual void h(){};
        virtual void y(){h();};
        virtual void e(){};
      };

      class F {
      public:
        virtual void s(){};
      };
    };

    class S {
    public:
      virtual void b(){};
      virtual void l(){b();};
    };

    class X {
    public:
      virtual void t(){};
    };

    class N {
    public:
      virtual void c(){e();};
      virtual void f(){};
      virtual void e(){f();};
    };
};

class V : public H::P::I{
  public:
    virtual void h(){y();};
    virtual void e(){};
    virtual void o(){};
    virtual void q(){o();};
};

