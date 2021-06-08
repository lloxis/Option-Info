let liste = [1; 2; 3; 4; 5; 6];;

let rec somme l =
  match l with
  |[] -> 0
  |t::q -> t + (somme q);;

somme liste;;

let somme_term l =
  let rec aux som l1 =
    match l1 with
    |[] -> som
    |t::q -> aux (som+t) q in
  aux 0 l;;

somme_term liste;;

let factoriel_it n =
  let p = ref 1 in
  for k = 2 to n do
    p := !p * k done;
  !p;;

let rec factoriel n =
  match n with
  |0 -> 1
  |_ -> n * (factoriel (n-1));;

factoriel 3;;
factoriel 4;;

let factoriel_term n =
  let rec aux fact n =
    match n with
    |0 -> fact
    |_ -> aux (n*fact) (n-1) in
  aux 1 n;;

factoriel_term 4;;

let rec somme a b =
  match b with
  |0 -> a
  |_ -> 1 + somme a (b-1);;

somme 5 6;;

let somme_term a b = 
  let rec aux som b =
    match b with
    |0 -> som
    |_ -> aux (som+1) (b-1) in
  aux a b;;

somme_term 5 6;;

let somme_cubes_it n =
  let somme = ref 0 in
  for k = 1 to n do
    somme := !somme + k*k*k done;
  !somme;;

somme_cubes_it 6;;

let rec somme_cubes_rec n =
  match n with
  |1 -> 1
  |_ -> n*n*n + somme_cubes_rec (n-1);;

somme_cubes_rec 6;;

let somme_cubes_term n =
  let rec aux som n =
    match n with
    |0 -> som
    |_ -> aux (n*n*n+som) (n-1) in
  aux 0 n;;

somme_cubes_term 6;;

(* est fibo *)

let rec fibo n =
  match n with
  |0 -> 0
  |1 -> 1
  |_ -> fibo (n-1) + fibo (n-2);;

fibo 10;;

let fibo_it n =
  let a = ref 0 in
  let b = ref 1 in
  for k=1 to n do
    let c = !a in
    a := !b;
    b := c + !b;
  done;
  !a;;

let fibo_rec_term n =
  let rec aux a b n =
    match n with
    |1 -> b
    |_ -> aux (b) (a+b) (n-1) in
  aux 0 1 n;;

fibo_rec_term 10;;

let rec somme_chiffes n =
  match n with
  |0 -> 0
  |_ -> (n mod 10) + somme_chiffes (n/10);;

somme_chiffes 123;;

let somme_chiffes_term n =
  let rec aux som n =
    match n with
    |0 -> som
    |_ -> aux (n mod 10 +som) (n/10) in
  aux 0 n;;

somme_chiffes_term 123;;

(* let somme_chiffes_it n =
  let ref som = 0 in
  for 
  match n with
  |0 -> 0
  |_ -> (n mod 10) + somme_chiffes (n/10);;

somme_chiffes_it 123;; *)

