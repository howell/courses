package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Int extends Expression {
    public final int x;

    public Int(int x) {
        this.x = x;
    }

    @Override
    public Int eval() {
        return this;
    }
}

