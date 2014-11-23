package hw7;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by scaldwell on 11/22/14.
 */
public class Environment {

    private Map<String, GeometryValue> m_env;

    public Environment() {
        m_env = new HashMap<String, GeometryValue>();
    }

    public Environment(String s, GeometryValue v, Environment env) {
        this();
        m_env.putAll(env.m_env);
        m_env.put(s, v);
    }

    public Environment extend(String s, GeometryValue v) {
        return new Environment(s, v, this);
    }

    public GeometryValue lookup(String s) throws UnboundVariableException {
        GeometryValue v = m_env.get(s);
        if (v == null) {
            throw new UnboundVariableException(s);
        }
        return v;
    }
}
