package mutualrec.nonterm;

class Base {
    public int x;
    public void n(int v) {
        System.out.printf("Calling base.n with v = %d\n%n", v);
        x = v;
    }

    public void m(int v) {
        System.out.printf("Calling base.m with v = %d\n%n", v);
        n(v);
    }
}

class Derived extends Base {
    public void n(int v) {
        System.out.printf("Calling derived.n with v = %d\n", v);
        m(v);
    }
}

public class MutualRecNonTerm {
    public Base b;
    public Derived d;

    public MutualRecNonTerm(Base b, Derived d) {
        this.b = b;
        this.d = d;
    }

    public void dataize() {
        b.n(10);
        System.out.printf("base:\n\tx after n = %d\n", b.x);
        b.m(12);
        System.out.printf("\tx after m = %d\n", b.x);
        d.n(5);
        System.out.printf("\nderived:\n\tx after n = %d\n", d.x);
    }

    public static void main(String[] args) {
        MutualRecNonTerm app = new MutualRecNonTerm(new Base(), new Derived());
        app.dataize();
    }

}