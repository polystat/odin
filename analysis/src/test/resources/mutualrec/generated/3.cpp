
// l.d -> j.k -> l.d
// j.u -> l.d -> j.k -> l.d
// j.t -> j.u -> l.d -> j.k -> l.d
// j.k -> l.d -> j.k


class L {
  public:
    virtual void t(){d();};
    virtual void d(){k();};
    virtual void k(){};
};

class P : public L{
  public:
    virtual void k(){n();};
    virtual void w(){};
    virtual void n(){w();};
    virtual void d(){};
    virtual void t(){w();};
};

class J : public L{
  public:
    virtual void u(){d();};
    virtual void t(){u();};
    virtual void k(){d();};
};
