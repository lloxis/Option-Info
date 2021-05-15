let liste = [1; 2; 3; 4; 5; 6; 7; 8];;

let rec long l =
  match l with
  |[] -> 0
  |t::q -> 1 + long q;;

long liste;;

let rec somme l =
  match l with
  |[] -> 0
  |t::q -> t + somme q;;

somme liste;;

let rec test_pres l x = 
  match l with
  |[] -> false
  |t::q -> t=x || test_pres q x;;

test_pres liste 1;;
test_pres liste 0;;

let rec avant_dern l =
  match l with
  |[] -> failwith "liste vide"
  |[x] -> failwith "trop petit"
  |[a; b] -> a
  |t::q -> avant_dern q;;

avant_dern liste;;
avant_dern [];;

let rec map_liste l f =
  match l with
  |[] -> []
  |t::q -> (f t)::(map_liste q f);;

let f x = x*2;;

map_liste liste f;;

let rec test_cst_x l x =
  match l with
  |[] -> true
  |t::q -> t=x && (test_cst_x q x);;

test_cst_x liste 1;;
test_cst_x [1; 1; 1; 1] 1;;

let rec test_cst l =
  match l with
  |[] -> true
  |[x] -> true
  |t1::t2::q -> (t1=t2) && (test_cst (t2::q));;

test_cst liste;;
test_cst [1; 1; 1; 1];;

let rec test_croiss l =
  match l with
  |[] -> true
  |[x] -> true
  |t1::t2::q -> (t1<=t2) && (test_croiss (t2::q));;

test_croiss liste;;
test_croiss [1; 1; 1; 1];;
test_croiss [1; 2; 0; 1];;

(* retourner une liste *)
(* trop de complexité nul *)
let rec mirror_catastrophe l =
  match l with
  |[] -> []
  |t::q -> (mirror_catastrophe q)@[t];;

mirror_catastrophe liste;;

let mirror l =
  let rec deplacer liste_fait liste_a_faire =
    match liste_a_faire with
    |[] -> liste_fait
    |t::q -> deplacer (t::liste_fait) q in
  deplacer [] l;;

mirror liste;;

let rec test_decroiss l =
  match l with
  |[] -> true
  |[x] -> true
  |t1::t2::q -> (t1>=t2) && (test_croiss (t2::q));;

let rec test_mono l = (test_croiss l)||(test_decroiss l);;

let rec max_liste l =
  match l with
  |[] -> failwith "liste vide"
  |[x] -> x
  |t::q -> let max_q = max_liste q in
    if t>=max_q then t
    else max_q;;

max_liste liste;;

let ind_max l =
  match l with
  |[] -> failwith "liste vide"
  |[x] -> x
  |t::q -> let max_q = max_liste q in
    if t>=max_q then t
    else max_q;;

let ajout_deb l x = x::l;;

ajout_deb liste 1;;

let rec ajout_fin l x = 
  match l with
  |[] -> [x]
  |t::q -> t::(ajout_fin q x);;

ajout_fin liste 1;;

let rec supprim_fin l =
  match l with
  |[] -> failwith "liste vide"
  |[x] -> []
  |t::q -> t::(supprim_fin q);;

supprim_fin liste;;

let rec supprim_deb l =
  match l with
  |[] -> failwith "liste vide"
  |[x] -> []
  |t::q -> q;;

supprim_deb liste;;

let rec enleve l x =
  match l with
  |[] -> failwith "liste vide"
  |[a] when a=x -> []
  |[a] -> [a]
  |t::q when t=x -> q
  |t::q -> t::(enleve q x);;

enleve liste 5;;

let rec concat l1 l2 = 
  match l1, l2 with
  |[], _ -> l2
  |_, [] -> l1
  |t1::q1, _ -> t1::(concat q1 l2);;

concat liste liste;;

(* fusion pas récursive terminale *)
let rec fusion2 l1 l2 =
  match l1, l2 with
  |[], _ -> l2
  |_, [] -> l1
  |t1::q1, t2::q2 when t1 <= t2 -> t1::(fusion q1 l2)
  |t1::q1, t2::q2 -> t2::(fusion l1 q2);;

(* fusion récursive terminale *)
let rec fusion l1 l2 =
  let rec aux l1 l2 sortie =
    match l1, l2 with
    |[], [] -> l2
    |[], h2::q2 -> aux [] q2 (h2::sortie)
    |h1::q1, [] -> aux [] q1 (h1::sortie)
    |t1::q1, t2::q2 when t1 <= t2 -> aux q1 l2 (t1::sortie)
    |t1::q1, t2::q2 -> aux l1 q2 (t2::sortie)
  miror (aux l1 l2 []);;

let rec union l1 l2 =
  match l1, l2 with
  |_, [] -> l1
  |[], _ -> l2
  |t::q, _ when test_pres l2 t -> union q l2
  |t::q, _ -> t::(union q l2);;

union liste (10::liste);;

let rec supprime_n l n =
  match l with
  |[] -> []
  |t::q -> supprime_n q (n-1);;

(* exemple: suffixes de [1; 2; 3] : [], [3], [2; 3], [1; 2; 3] *)
let rec suff_liste l =
  match l with
  |[] -> [[]]
  |t::q -> l::(suff_liste q);;

suff_liste liste;;

(* foncion auxiliaire pour pref_liste ; parametre : liste de listes *)
let rec ajoute_devant_chaque l x =
  match l with
  |[] -> []
  |t::q -> (ajout_deb t x)::(ajoute_devant_chaque q x)

(* exemple: preffixes de [1; 2; 3] : [], [1], [1; 2], [1; 2; 3] *)
let rec pref_liste l =
  match l with
  |[] -> [[]]
  |t::q -> []::(ajoute_devant_chaque (pref_liste q) t);;

pref_liste liste;;


