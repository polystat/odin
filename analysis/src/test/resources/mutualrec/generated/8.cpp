
// g.y -> d.i -> g.y
// d.i -> g.y -> d.i


class G {
  public:
    virtual void n(){};
    virtual void y(){i();};
    virtual void k(){n();};
    virtual void i(){n();};
};

class U : public G{
  public:
    virtual void k(){};
    virtual void w(){};
};

class D : public G{
  public:
    virtual void u(){};
    virtual void k(){n();};
    virtual void n(){};
    virtual void i(){y();};
    virtual void q(){};
};
