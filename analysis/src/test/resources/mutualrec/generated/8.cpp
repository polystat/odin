// s.v -> u.e -> s.v
// s.m -> s.v -> u.e -> s.v
// u.e -> s.v -> u.e

class U {
  public:
    virtual void e(){v();};
    virtual void v(){m();};
    virtual void u(){};
    virtual void m(){};
};

class S : public U{
  public:
    virtual void j(){};
    virtual void v(){e();};
    virtual void m(){v();};
};

class O {
  public:
    virtual void a(){};
    virtual void p(){};
};

