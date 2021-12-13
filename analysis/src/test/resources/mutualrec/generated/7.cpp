
// w.l -> t.e -> w.l
// t.e -> w.l -> t.e


class W {
  public:
    virtual void l(){e();};
    virtual void e(){};
};

class V : public W{
  public:
    virtual void o(){};
    virtual void m(){};
    virtual void e(){};
};

class T : public V{
  public:
    virtual void o(){};
    virtual void r(){};
    virtual void m(){};
    virtual void e(){l();};
};
