let rec puiss a n =
  match n with
  |0 -> 1
  |_ -> a * (puiss a (n-1));;

puiss 2 4;;

let rec expo_rap a n =
  match n with
  |0 -> 1
  |_ -> let b = puiss a (n/2) in
      if (n mod 2) = 0 then b * b
      else b * b * a;;

expo_rap 2 4;;



let rec fact n =
  match n with
  |1 -> 1
  |_ -> n * (fact (n-1));;

fact 6;;

let rec syr_rec n = match n with
|1 -> 0
|k when k mod 2 = 0 -> 1 + syr_rec (n/2)
|_ -> 1 + syr_rec (3 * n + 1);;

let rec somme_puiss n p =
  match n with
  |0 -> 0
  |_ -> (expo_rap n p) + (somme_puiss (n-1) p);;

let rec fibo_rec n =
  match n with
  |0 -> 0
  |1 -> 1
  |_ -> (fibo_rec (n-1)) + (fibo_rec (n-2));;

(* paramétres : nombre de disques, tour de depart, tour d'arrivée, tour en plus *)
let rec hanoi n a b c =
  match n with
  |1 -> [(a, b)]
  |_ -> (hanoi (n-1) a c b)@((a, b)::(hanoi (n-1) c b a));;

hanoi 4 "1" "3" "2";;

(* let rec heron n =
  match n with
  |0 -> 1.
  |_ -> 0.5*.(1.+.2./.(heron (n-1))) *)

let rec suite_rec f a n =
match n with
|0 -> a
|_ -> f (suite_rec f a (n-1))

let rec modulo n d =
  if n < d then n
  else modulo (n-d) d;;

modulo 41 2;;

let rec pgcd a b =
  match b with
  |0 -> a
  |_ -> pgcd b (modulo a b);;

let rec test_palin str =
  let n = String.length str in
  match n with
  |0 -> true
  |1 -> true
  |_ -> ( str.[0] = str.[n-1] ) && test_palin(String.sub str 1 (n-2));;

test_palin "lol";;
test_palin "loul";;

(* on utillise cos((n+1)x) = cos(nx)cos(x) - sin(nx)sin(x) *)
(* on aurait pu utiliser cos((n+1)x) = 2cos(x)cos(nx) - cos((n-1)x) *)
let rec tchebychev cosx sinx n =
  match n with
  |0 -> 1., 0.
  |1 -> cosx, sinx
  |_ -> let cosnx, sinnx = tchebychev cosx sinx (n-1) in
    cosx*.cosnx -. sinnx*.sinx, sinnx*.cosx +. cosnx+.sinx;;



let rec iteration f n x =
  match n with
  |0 -> x
  |_ -> iteration f (n-1) (f x);;

let g = iteration cos 50;;
g 0.7;;