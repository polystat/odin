// t.w.d -> q.t -> t.w.d
// q.h -> t.w.d -> q.t -> t.w.d
// q.t -> t.w.d -> q.t

class T {
  public:
    virtual void q(){};
    virtual void g(){v();};
    virtual void v(){};

    class Y {
    public:
      virtual void u(){};
    };

    class W {
    public:
      virtual void d(){t();};
      virtual void t(){};

      class H {
      public:
        virtual void z(){};
        virtual void m(){z();};
        virtual void x(){m();};
      };
    };
};

class Q : public T::W{
  public:
    virtual void h(){d();};
    virtual void t(){d();};

    class H {
    public:
      virtual void z(){};
      virtual void m(){z();};
      virtual void x(){m();};
    };
};

