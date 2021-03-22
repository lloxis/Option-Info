(* permier element à gauche de la liste correspond au 2 puissance 0 de la décomposition binaire *)

let rec binaire n = 
  if n > 0 then (n mod 2)::(binaire (n/2))
  else [];;

binaire 100;;

let rec entier list = 
  match list with
  |[] -> 0
  |t::q -> t+2*(entier q);;


entier [0; 0; 1; 0; 0; 1; 1];;

let rec gray n =
  (gray (n-1)).length