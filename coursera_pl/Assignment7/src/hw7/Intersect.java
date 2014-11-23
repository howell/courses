package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Intersect extends GeometryExpression {
    private final GeometryExpression e1, e2;

    public Intersect(GeometryExpression e1, GeometryExpression e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public GeometryValue eval(Environment env) throws GeometryException {
        return e1.eval(env).intersect(e2.eval(env));
    }

    @Override
    public Intersect preprocess() {
        return new Intersect(e1.preprocess(), e2.preprocess());
    }
}
