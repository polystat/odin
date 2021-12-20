// u.z -> l.g -> u.z
// l.d -> u.z -> l.g -> u.z
// l.g -> u.z -> l.g

class U {
  public:
    virtual void g(){};
    virtual void t(){};
    virtual void z(){g();};
};

class L : public U{
  public:
    virtual void d(){z();};
    virtual void g(){z();};
};

