/**
 * Created by scaldwell on 1/30/15.
 */

class Foo {

}

class Bar extends Foo {

}

public class JavaArrayFail{
    public static void main(String args[])
    {
        Foo[] bars = new Bar[1];
        bars[0] = new Foo();
    }
}
