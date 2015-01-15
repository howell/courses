/* Examples for testing */

 lambda x:Bool. x;
 (lambda x:Bool->Bool. if x false then true else false) 
   (lambda x:Bool. if x then false else true); 

(lambda x:Bool. let y = true in (if x then false else true)) false;

(lambda x: Bool. let y = x in (let y = false in y)) true;
