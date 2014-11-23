package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class LineSegment extends GeometryValue {
    public final double x1, y1, x2, y2;

    public LineSegment(double x1, double y1, double x2, double y2) {
        this.x1 = x1;
        this.y1 = y1;
        this.x2 = x2;
        this.y2 = y2;
    }

    public LineSegment flip() {
        return new LineSegment(x2, y2, x1, y1);
    }

    @Override
    public GeometryExpression preprocess() {
        if (realClosePoint(x1, y1, x2, y2)) {
            return new Point(x1, y1);
        } else if (realClose(x1, x2)) {
            if (y1 < y2) {
                return this;
            } else {
                return flip();
            }
        } else if (x1 < x2) {
            return this;
        } else {
            return flip();
        }
    }

    @Override
    public LineSegment shift(double dx, double dy) {
        return new LineSegment(x1 + dx, y1 + dy, x2 + dx, y2 + dy);
    }

    @Override
    public GeometryValue intersect(GeometryValue other) {
        return other.intersectLineSegment(this);
    }

    @Override
    public GeometryValue intersectPoint(Point point) {
        return point.intersectLineSegment(this);
    }

    @Override
    public GeometryValue intersectLine(Line line) {
        return line.intersectLineSegment(this);
    }

    @Override
    public GeometryValue intersectVerticalLine(VerticalLine vline) {
        return vline.intersectLineSegment(this);
    }

    @Override
    public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
        if (realClose(seg.x1, seg.x2)) {
            // segments are on a vertical line
            // let segment a start at or below start of segment b
            LineSegment segA, segB;
            if (seg.y1 < y2) {
                segA = seg;
                segB = this;
            } else {
                segA = this;
                segB = seg;
            }
            if (realClose(segA.y2, segB.y1)) {
                return new Point(segA.x2, segA.y2); // just touching
            } else if (segA.y2 < segB.y1) {
                return NoPoints.NoPoints;   // disjoint
            } else if (segA.y2 > segB.y2) {
                return segB;    // b inside a
            } else {
                return new LineSegment(segB.x1, segB.y1, segA.x2, segA.y2); // overlapping
            }
        } else {
            // the segments are on a (non-vertical) line
            // let segment a start at or to the left of start of segment b
            LineSegment segA, segB;
            if (seg.x1 < x1) {
                segA = seg;
                segB = this;
            } else {
                segA = this;
                segB = seg;
            }
            if (realClose(segA.x2, segB.x2)) {
                return new Point(segA.x2, segB.y2); // just touching
            } else if (segA.x2 < segB.x1) {
                return NoPoints.NoPoints;   // disjoint
            } else if (segA.x2 > segB.x2) {
                return segB;    // b inside a
            } else {
                return new LineSegment(segB.x1, segB.y1, segA.x2, segA.y2); // overlapping
            }
        }
    }

}
