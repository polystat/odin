
// q.t -> k.y -> q.t
// k.h -> k.y -> q.t -> k.y
// k.y -> q.t -> k.y
// q.k -> k.h -> k.y -> q.t -> k.y


class K {
  public:
    virtual void y(){t();};
    virtual void h(){y();};
    virtual void t(){};
};

class F {
  public:
    virtual void o(){};
    virtual void w(){};
};

class Q : public K{
  public:
    virtual void t(){y();};
    virtual void v(){};
    virtual void k(){h();};
};
