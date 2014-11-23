package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Var extends GeometryExpression {
    private final String s;

    public Var(String s) {
        this.s = s;
    }

    @Override
    public GeometryValue eval(Environment env) throws UnboundVariableException {
        return env.lookup(s);
    }

    @Override
    public Var preprocess() {
        return this;
    }
}
