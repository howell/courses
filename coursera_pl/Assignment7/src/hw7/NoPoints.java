package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class NoPoints extends GeometryValue {

    public static final NoPoints NoPoints = new NoPoints();

    private NoPoints() {

    }

    @Override
    public NoPoints shift(double dx, double dy) {
        return this;
    }

    @Override
    public GeometryValue intersect(GeometryValue other) {
        return other.intersectNoPoints(this);
    }

    @Override
    public GeometryValue intersectPoint(Point point) {
        return this;
    }

    @Override
    public GeometryValue intersectLine(Line line) {
        return this;
    }

    @Override
    public GeometryValue intersectVerticalLine(VerticalLine vline) {
        return this;
    }

    @Override
    public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
        return this;
    }

}
