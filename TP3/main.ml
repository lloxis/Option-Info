type polynome = (int * int) list;;

let rec validation (p:polynome) =
  match p with
  |[] -> false
  |[(a, n)] -> a <> 0
  |(a, n)::(b, m)::reste -> a <> 0 && b <> 0 && n > m && validation reste;;

validation [(1, 3); (1, 2); (2, 2)];;
validation [(1, 3); (1, 2); (0, 2)];;

let rec puissance a n =
  if n <= 0 then 1
  else
  a * puissance a (n-1);;

puissance 2 4;;

let rec evaluation p a =
  match p with
  |[] -> 0
  |(b, n)::reste -> b * puissance a n + evaluation reste a ;;

evaluation [(1, 0); (1, 2)] 3;;

let rec plus p1 p2 =
  match p1, p2 with
  |p, [] -> p
  |[], p -> p
  |(a, n)::reste1, (b, m)::reste2 when n > m -> (a, n)::(plus reste1 p2)
  |(a, n)::reste1, (b, m)::reste2 when m > n -> (b, m)::(plus p1 reste2)
  |(a, n)::reste1, (b, m)::reste2 when a + b = 0 -> (plus reste1 reste2)
  |(a, n)::reste1, (b, m)::reste2 -> (a+b, n)::(plus reste1 reste2);;

plus [(1, 0)] [(1, 2); (1, 0)];;

let rec p_monome p1 p2 =
  match p1, p2 with
  |(a, n), [] -> p2
  |(a, n), (b, m)::reste -> (a*b, n+m)::(p_monome (a, n) reste);;

p_monome (2, 0) [(1, 2); (3, 0)];;
p_monome (2, 1) [(1, 2); (3, 0)];;

let rec fois p1 p2 =
  match p1, p2 with
  |[], p2 -> []
  |(a, n)::reste, p2 -> plus (p_monome (a, n) p2) (fois reste p2);;

fois [(1, 1); (-1, 0)] [(1, 1); (1, 0)];;


let rec derivee p =
  match p with
  |[] -> []
  |(a, n)::reste when n > 0 -> (a*n, n-1)::(derivee reste)
  |(a, n)::reste -> (derivee reste);;

derivee [(1, 3); (2, 2); (1, 0)];;


let rec tch n =
  if n = 0 then [(1, 0)]
  else if n = 1 then [(1, 1)]
  else plus (p_monome (2, 1) (tch (n-1))) (p_monome (-1, 0) (tch (n-2)));;

tch 9;;

let rec division a b =
  match a, b with
  |[], b -> ([],[])
  |(a1, p)::reste, (b1, q)::reste2 when p < q -> ([], a)
  |(a1, p)::reste, (b1, q)::reste2 ->
    let aa = plus a (p_monome (-a1, p-q)  b) in
    let q1, r1 = division aa b in
    (plus [(a1, p-q)] q1), r1;;

division [(1, 2); (-1, 0)] [(1, 1); (-1, 0)];;