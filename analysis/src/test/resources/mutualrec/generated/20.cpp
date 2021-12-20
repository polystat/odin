// q.d -> q.c -> a.r -> q.e -> q.d
// q.e -> q.d -> q.c -> a.r -> q.e
// q.c -> a.r -> q.e -> q.d -> q.c
// a.r -> q.e -> q.d -> q.c -> a.r

class A {
  public:
    virtual void r(){e();};
    virtual void c(){};
    virtual void e(){};

    class A {
    public:
      virtual void a(){};
    };
};

class Q : public A{
  public:
    virtual void d(){c();};
    virtual void e(){d();};
    virtual void i(){};
    virtual void c(){r();};
};

