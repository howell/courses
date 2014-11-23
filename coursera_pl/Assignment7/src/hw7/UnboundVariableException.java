package hw7;

/**
 * Created by scaldwell on 11/22/14.
 */
public class UnboundVariableException extends GeometryException {
    public final String var;
    public final String message;

    public UnboundVariableException(String var) {
        this.var = var;
        this.message = "Tried to reference unbound variable " + var;
    }

}
