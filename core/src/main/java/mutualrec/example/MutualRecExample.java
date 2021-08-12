package mutualrec.example;

class Base {
    public int x;

    public void f(int v) {
        x = v;
    }

    public void g(int v) {
        f(v);
    }
}

class Derived extends Base {
    @Override
    public void f(int v) {
        g(v);
    }
}


public class MutualRecExample {
    public static void main(String[] args) {
        Derived app = new Derived();
        app.f(10);
    }
}
