fun filter(p, xs) =
    if null xs
    then []
    else let val x = hd xs
             val xs_ = filter (p, tl xs)
         in
             if p x
             then x :: xs_
             else xs_
         end
