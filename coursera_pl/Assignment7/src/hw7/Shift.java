package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Shift extends GeometryExpression {
    private final double dx, dy;
    private final GeometryExpression e;

    public Shift(double dx, double dy, GeometryExpression e) {
        this.dx = dx;
        this.dy = dy;
        this.e = e;
    }

    @Override
    public GeometryValue eval(Environment env) throws GeometryException {
        return e.eval(env).shift(dx, dy);
    }

    @Override
    public Shift preprocess() {
        return new Shift(dx, dy, e.preprocess());
    }
}
