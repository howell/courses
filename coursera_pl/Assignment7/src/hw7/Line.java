package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Line extends GeometryValue {
    public final double m;
    public final double b;

    public Line(double m, double b) {
        this.m  = m;
        this.b = b;
    }

    @Override
    public Line shift(double dx, double dy) {
        return new Line(m, b + dy - m * dx);
    }

    @Override
    public GeometryValue intersect(GeometryValue other) {
        return other.intersectLine(this);
    }

    @Override
    public GeometryValue intersectPoint(Point point) {
        return point.intersectLine(this);
    }

    @Override
    public GeometryValue intersectLine(Line line) {
        if (realClose(m, line.m)) {
            if (realClose(b, line.b)) {
                return this;
            } else {
                return NoPoints.NoPoints;
            }
        } else {
            double x = (line.b - b) / (m - line.m);
            double y = m * x + b;
            return new Point(x, y);
        }
    }

    @Override
    public GeometryValue intersectVerticalLine(VerticalLine vline) {
        double x = vline.x;
        return new Point(x, m * x + b);
    }

    @Override
    public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
        return seg;
    }

}
