package hw7;

/**
 * Created by scaldwell on 11/23/14.
 */
public class EString extends Expression {
    public final String s;

    public EString(String s) {
        this.s = s;
    }

    @Override
    public Expression eval() {
        return this;
    }
}
