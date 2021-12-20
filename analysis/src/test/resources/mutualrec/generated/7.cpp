// q.d -> i.v -> q.d
// i.v -> q.d -> i.v

class Q {
  public:
    virtual void a(){};
    virtual void v(){a();};
    virtual void d(){v();};

    class J {
    public:
      virtual void t(){};
      virtual void o(){};
      virtual void d(){t();};
    };
};

class I : public Q{
  public:
    virtual void i(){a();};
    virtual void v(){d();};
};

