// v.z -> w.q -> v.z
// w.a -> v.z -> w.q -> v.z
// w.s -> v.z -> w.q -> v.z
// w.q -> v.z -> w.q

class E {
  public:
    virtual void m(){s();};
    virtual void s(){};
    virtual void v(){s();};
};

class V {
  public:
    virtual void z(){q();};
    virtual void q(){};
};

class W : public V{
  public:
    virtual void a(){z();};
    virtual void s(){z();};
    virtual void q(){z();};
};

