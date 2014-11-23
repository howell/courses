package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class VerticalLine extends GeometryValue {
    public final double x;

    public VerticalLine(double x) {
        this.x = x;
    }

    @Override
    public VerticalLine shift(double dx, double dy) {
        return new VerticalLine(x + dx);
    }

    @Override
    public GeometryValue intersect(GeometryValue other) {
        return other.intersect(this);
    }

    @Override
    public GeometryValue intersectPoint(Point point) {
        return point.intersectVerticalLine(this);
    }

    @Override
    public GeometryValue intersectLine(Line line) {
        return line.intersectVerticalLine(this);
    }

    @Override
    public GeometryValue intersectVerticalLine(VerticalLine vline) {
        if (realClose(x, vline.x)) {
            return this;
        } else {
            return NoPoints.NoPoints;
        }
    }

    @Override
    public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
        return seg;
    }

}
