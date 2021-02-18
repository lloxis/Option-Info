let f1 a b = a+b;;
let f2 a = a 1 + 2;;
let f3 a b = 
  if a +. 0. = 2. && b
    then 0
  else 0;;
let f4 x f = if f 3 = 5 then x+.7.3 else 1.;;
(* 2 possibilitées pour f5 mais avant la curification on n'a pas les parenthèses *)
let f5 a = fun b -> b || (a = 1);;
let f5_2 a =
  let h b = b || (a = 1) in h;;
let f6 (a, b) = a = 2 && b;;
let f7 (a, b, c) = a = 0 && b = 0. && c;;