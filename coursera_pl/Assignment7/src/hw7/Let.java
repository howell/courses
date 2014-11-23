package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Let extends GeometryExpression {
    private final String s;
    private final GeometryExpression e1, e2;

    public Let(String s, GeometryExpression e1, GeometryExpression e2) {
        this.s = s;
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public GeometryValue eval(Environment env) throws GeometryException {
        Environment newEnv = env.extend(s, e1.eval(env));
        return e2.eval(newEnv);
    }

    @Override
    public Let preprocess() {
        return new Let(s, e1.preprocess(), e2.preprocess());
    }
}
