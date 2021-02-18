let f1 a b c = a=0 && b=0 && c=0;;
f1 1 0 0;;
f1 0 0 0;;

let f2 a b =
  if a > b
  then a
  else b;;

f2 2 3;;
f2 6 0;;

let f3 a b c = f2 a (f2 b c);;

f3 1 2 3;;
f3 3 4 3;;

let f4 pol =
  let (a, b, c) = pol in
  b*.b-.4.*.a*.c;;

let f5 x = (2.*.x)/.(1.+.x*.x);;
f5 0.0;;
f5 2.;;

let f6 a =
  if a > 0.
    then a
  else -.a;;

f6 2.;;
f6 (-.1.);;

let f7 a b c = a = b && b = c;;
f7 0. 1. 1.;;
f7 1. 1. 1.;;

let f8 x = f2 x (1./.x);;
f8 0.5;;
f8 2.;;