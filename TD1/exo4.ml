let a = cos (sqrt 2.) in
  a +. a**3. +. 7.*.a**5.;;
let x = exp 3. in
  (3. +. x +. 7. *. x**2. )/.(x**2. -. 5. *. x**2. +. 2.);;
let f x = cos (x**2. +. x +. x +. 1.);;
let g x = f (f x);;
g (log 3.);;