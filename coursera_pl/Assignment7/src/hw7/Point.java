package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Point extends GeometryValue {
    public final double x;
    public final double y;

    public Point(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public boolean realClose(Point p) {
        return realClose(x, p.x) && realClose(y, p.y);
    }

    @Override
    public Point shift(double dx, double dy) {
        return new Point(x + dx, y + dy);
    }

    @Override
    public GeometryValue intersect(GeometryValue other) {
        return other.intersectPoint(this);
    }

    @Override
    public GeometryValue intersectPoint(Point point) {
        if (realClose(point)) {
            return this;
        } else {
            return NoPoints.NoPoints;
        }
    }

    @Override
    public GeometryValue intersectLine(Line line) {
        if (realClose(y, line.m * x + line.b)) {
            return this;
        } else {
            return NoPoints.NoPoints;
        }
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
        if (inBetween(x, seg.x1, seg.x2) && inBetween(y, seg.y1, seg.y2)) {
            return this;
        } else {
            return NoPoints.NoPoints;
        }
    }

    private boolean inBetween(double v, double end1, double end2) {
        return (end1 - Epsilon <= v && v <= end2 + Epsilon) || (end2 - Epsilon <= v && v <= end1 + Epsilon);
    }

}
