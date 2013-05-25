fun add_all(xs : int list) = 
    if null xs
    then 0 
    else hd xs + add_all(tl xs)
fun mult_list(xs : int list) =
    if null xs then 1 else hd(xs)*mult_list(tl xs)
