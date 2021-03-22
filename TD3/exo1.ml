let somme n =
  let resultat = ref 0 in
  for i = 1 to n do
      resultat := !resultat + i done;
  !resultat;;

somme 5;;

let somme_puiss n p =
  let resultat = ref 0. in
  for i = 1 to n do
      resultat := !resultat +. (float_of_int i +. 0.)**p done;
  !resultat;;
somme_puiss 5 1.;;

let seuil x =
  let resultat = ref 1. in
  let i = ref 1. in
  while !resultat <= x do
      i := !i +. 1.;
      resultat := !resultat +. (1. /. !i) done;
  int_of_float !i;;

  seuil 1.5;;

let somme2 n =
  let resultat = ref 0 in
  for i = 1 to n do
      if i mod 2 <> 0 && i mod 3 <> 0
      then resultat := !resultat + i done;
  !resultat;;
somme2 7;;