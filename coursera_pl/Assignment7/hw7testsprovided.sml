(* University of Washington, Programming Languages, Homework 7
   hw7testsprovided.sml *)
(* Will not compile until you implement preprocess and eval_prog *)

(* These tests do NOT cover all the various cases, especially for intersection *)

use "hw7.sml";

(* Must implement preprocess_prog and Shift before running these tests *)

fun real_equal(x,y) = Real.compare(x,y) = General.EQUAL;

(* Preprocess tests *)
let
	val Point(a,b) = preprocess_prog(LineSegment(3.2,4.1,3.2,4.1))
	val Point(c,d) = Point(3.2,4.1)
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "preprocess converts a LineSegment to a Point successfully\n")
	else (print "preprocess does not convert a LineSegment to a Point succesfully\n")
end;

let 
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,4.1,~3.2,~4.1))
	val LineSegment(e,f,g,h) = LineSegment(~3.2,~4.1,3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "preprocess flips an improper LineSegment successfully\n")
	else (print "preprocess does not flip an improper LineSegment successfully\n")
end;

(* eval_prog tests with Shift*)
let 
	val Point(a,b) = (eval_prog (preprocess_prog (Shift(3.0, 4.0, Point(4.0,4.0))), []))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with empty environment worked\n")
	else (print "eval_prog with empty environment is not working properly\n")
end;

(* Using a Var *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with 'a' in environment is working properly\n")
	else (print "eval_prog with 'a' in environment is not working properly\n")
end;


(* With Variable Shadowing *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0)),("a",Point(1.0,1.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with shadowing 'a' in environment is working properly\n")
	else (print "eval_prog with shadowing 'a' in environment is not working properly\n")
end;

let val a = eval_prog (Shift (1.3, 2.4, NoPoints), [])
in
    case a of
        NoPoints => print "Shifting NoPoints is working properly\n"
      | _        => print "Shifting NoPoints is not working properly\n"
end;

let val Point p = Point (1.2, 3.4)
    val Point p' = eval_prog (Shift (0.0, 0.0, Point p), [])
in
    if real_close_point p p'
    then (print "Shifting Point by 0 is working properly\n")
    else (print "Shifting Point by 0 is not working properly\n")
end;

let val Point p = Point (1.2, 3.4)
    val (x, y)  = p
    val (dx, dy) = (7.9, 8.4)
    val Point p' = eval_prog (Shift (dx, dy, Point p), [])
in
    if real_close_point (x + dx, y + dy) p'
    then (print "Shifting Point is working properly\n")
    else (print "Shifting Point is not working properly\n")
end;

let val VerticalLine x = VerticalLine 3.7
    val dx = 17.3
    val VerticalLine x' = eval_prog (Shift(dx, 99.9, VerticalLine x), [])
in
    if real_close (x + dx, x')
    then (print "Shifting VerticalLine is working properly\n")
    else (print "Shifting VerticalLine is not working properly\n")
end;

let val (x1, y1, x2, y2) = (1.2, 3.4, 4.5, 6.7)
    val s = LineSegment (x1, y1, x2, y2)
    val (dx, dy) = (8.45, 9.65)
    val LineSegment (x1', y1', x2', y2') = eval_prog (Shift (dx, dy, s), [])
in
    if real_close (x1 + dx, x1') andalso
       real_close (x2 + dx, x2') andalso
       real_close (y1 + dy, y1') andalso
       real_close (y2 + dy, y2')
    then (print "Shifting LineSegment is working properly\n")
    else (print "Shifting LineSegment is not working properly\n")
end;

let val Line l = Line (98.4, ~32.0)
    val (m, b) = l
    val (dx, dy) = (~42.4, 16.7)
    val Line (m', b') = eval_prog (Shift (dx, dy, Line l), [])
in
    if real_equal (m, m') andalso
       real_close (b', b + dy - m * dx)
    then (print "Shifting Line is working properly\n")
    else (print "Shifting Line is not working properly\n")
end;
 
