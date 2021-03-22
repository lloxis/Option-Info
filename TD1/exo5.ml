let u n = n*n;;
let f1 u = fun n -> (u n+1) - (u n);;

let composition f g = fun x -> f (g (x +. 0.) +. 0.) +. 0.;;