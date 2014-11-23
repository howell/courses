package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public abstract class GeometryValue extends GeometryExpression {
    public abstract GeometryValue shift(double dx, double dy);
    public abstract GeometryValue intersect(GeometryValue other);
    public abstract GeometryValue intersectPoint(Point point);
    public abstract GeometryValue intersectLine(Line line);
    public abstract GeometryValue intersectVerticalLine(VerticalLine vline);
    public abstract GeometryValue intersectWithSegmentAsLineResult(LineSegment seg);

    @Override
    public GeometryValue eval(Environment env) {
        return this;
    }

    @Override
    public GeometryExpression preprocess() {
        return this;
    }

    protected final double Epsilon = 0.00001;

    protected boolean realClose(double a, double b) {
        return Math.abs(a - b) < Epsilon;
    }

    protected boolean realClosePoint(double x1, double y1, double x2, double y2) {
        return realClose(x1, x2) && realClose(y1, y2);
    }

    protected GeometryValue twoPointsToLine(double x1, double y1, double x2, double y2) {
        if (realClose(x1, x2)) {
            return new VerticalLine(x1);
        } else {
            double m = (y2 - y1) / (x2 - x1);
            double b = y1 - m * x1;
            return new Line(m, b);
        }
    }

    protected GeometryValue intersectNoPoints(NoPoints np) {
        return np;
    }

    protected GeometryValue intersectLineSegment(LineSegment seg) {
        GeometryValue lineResult = intersect(twoPointsToLine(seg.x1, seg.x2, seg.y1, seg.y2));
        return lineResult.intersectWithSegmentAsLineResult(seg);
    }
}
