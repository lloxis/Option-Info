let rec suite u0 n =
  if n = 0 then u0
  else
  let u = suite u0 (n-1) in
  (2. *. u +. 1. ) /. (3. +. u);;

suite 1. 10;;

let rec puissance a n =
  if n = 0 then 1
  else a*(puissance a (n-1));;

puissance 3 3;;

let rec ackermann m n =
  match m, n with
  |(0, p) -> p+1
  |(p, 0) -> ackermann (p-1) 1
  |(p, q) -> ackermann (p-1) (ackermann p (q-1));;

ackermann 10, 10;;

let rec binomial n p =
  if p = 0 || n=p then 1
  else
  (n * (binomial (n-1) (p-1)))/p;;

binomial 50 10;;

let rec pgcd a b =
  if b = 0 then a
  else
  pgcd b (a mod b);;

let rec catalan n =
  if n = 0 then 1
  else
  let s = ref 0 in
  for i = 0 to n-1 do
    s := !s + (catalan i) * (catalan (n-i-1)) done;
  !s;;


let rec maximum liste =
  match List.tl liste with
  |[] -> max
  |[a] -> a
  |t::q -> max t (maximum q);;

let rec carre liste =
  match liste with
  |[] -> []
  |t::q -> (t*t)::(carre q);;

carre [1; 2; 3; 4; 5];;

let rec applique f liste =
  match liste with
  |[] -> []
  |t::q -> (f t)::(applique f q);;

let rec selectionMax liste =
  match liste with
  |[] -> failwith "Liste vide"
  |[a] -> a, []
  |t::q when t > (fst (selectionMax q)) -> t, q
  |t::q -> (fst (selectionMax q)), t::(snd (selectionMax q));;

selectionMax [1; 2; 3; 6; 18; 9; 11; 11; 13];;

let rec positifs liste = 
  match liste with
  |[] -> []
  |t::q when t > 0 -> t::(positifs q)
  |t::q -> positifs q;;

positifs [1; -2; 3; 6; -18; 9; -11; -11; 13];;

let rec filtrer f liste = 
  match liste with
  |[] -> []
  |t::q when f t -> t::(filtrer f q)
  |t::q -> filtrer f q;;

let rec verifie f liste =
  match liste with
  |[] -> None
  |t::q when f t -> Some t
  |t::q -> verifie f q;;

let rec existe f liste =
  match liste with
  |[] -> false
  |t::q when f t -> true
  |t::q -> existe f q;;

let rec pour_tout f liste =
  match liste with
  |[] -> true
  |t::q -> f t && pour_tout f q;;

let rec prendre n liste =
  if n = 0 then [], liste
  else
  match liste with
  |[] -> [], []
  |list when List.length list <= n -> liste, []
  |t::q -> t::(fst (prendre (n-1) q)), (snd (prendre (n-1) q));;

prendre 5 [1; 2; 3; 4; 5; 6; 8; 10];;

let rec shuffle liste1 liste2 =
  match liste1, liste2 with
  |[], list -> list
  |list, [] -> list
  |t1::q1, t2::q2 -> t1::t2::(shuffle q1 q2);;

shuffle [13; 4; 11; 8] [9; 7; 13; 5; 12; 2];;

let rec parties liste =
  match liste with
  |[] -> []
  |t::q -> [[t]; t::q; q ]@(parties q);;

parties [1; 2; 1];;

let rec enum a b =
  if a > b then []
  else a::(enum (a+1) b);;

enum 50 100;;

let rec oterMult k liste =
  match liste with
  |[] -> []
  |t::q when t mod k = 0 -> oterMult k q
  |t::q -> t::(oterMult k q);;

oterMult 3 [1; 2; 3; 8; 6; 9; 10; 12];;

let crible n =
  (* let premiers = 1::(enum 2 n) in *)
  if n <= 0 then []
  else
  let rec enleve liste =
    match liste with
    |[] -> []
    |t::q -> t::(enleve (oterMult t q)) in
  enleve (enum 2 n);;

crible 100;;

let crible_ameliore n =
  (* let premiers = 1::(enum 2 n) in *)
  if n <= 0 then []
  else
  let rec enleve liste =
    match liste with
    |[] -> []
    |t::q -> t::(enleve (oterMult t q)) in
  enleve (enum 2 n);;

let rec est_sous_liste m1 m2 =
  match m1, m2 with
  |[], l1 -> true
  |l1, [] -> false
  |t1::q1, t2::q2 when t1 = t2 -> est_sous_liste q1 q2
  |t1::q1, t2::q2 -> est_sous_liste m1 q2;;

est_sous_liste [3; 3; 4; 5; 7; 9; 8; 8] [1; 2; 3; 3; 4; 5; 7; 9; 8; 8; 8; 8; 0; 100; 101; 103; 456; 9; 0];;
est_sous_liste [1; 2; 4; 3; 5; 3; 7; 9; 8; 8; 8; 8; -1] [1; 2; 3; 3; 4; 5; 7; 9; 8; 8; 8; 8; 0; 100; 101; 103; 456; 9; 0];;