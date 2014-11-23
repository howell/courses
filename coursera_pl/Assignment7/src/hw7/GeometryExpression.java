package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public abstract class GeometryExpression {

    public abstract GeometryValue eval(Environment env) throws GeometryException;
    public abstract GeometryExpression preprocess();

}
