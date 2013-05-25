fun gcd (x,y) =
  if x < y
  then gcd(x, y-x)
  else
    if x = y
    then x
    else gcd(y, x)

