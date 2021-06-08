let points = [(10.3, 5.4); ( 1.2, 2.5); ( 3.4, 2.7); (10.7, 2.4);
              ( 5.7, 1.1); ( 0.9, 5.7); ( 6.5, 7.8); ( 6.5, 4.1);
              (11.3, 6.8); ( 8.7, 2.9); ( 5.6, 6.2); ( 9.1, 5.0);
              ( 2.0, 8.9); ( 7.8, 2.2); ( 9.1, 0.1); ( 1.8, 5.0);
              ( 3.7, 1.8); ( 7.5, 7.1); (10.4, 6.4); ( 4.1, 8.3)];;

let distance p q =
  let x, y = p in
  let x', y' = q in
  sqrt ((x'-. x)**2.0 +. (y' -. y)**2.0);;

let rec plus_proche p liste =
  match liste with
  |[] -> failwith "plus_proche : liste vide"
  |[a] -> a, (distance p a)
  |t::q -> let min_point, min_dist = plus_proche t q in
  let dist_t = (distance p t) in
  if dist_t < min_dist then t, dist_t
  else min_point, min_dist;;

let rec points_proches p =
  match p with
  |[] -> failwith "points_proches : liste vide"
  |[a] -> failwith "points_proches : unique point"
  |[a, b] -> a, b, (distance a b)
  |t::q -> let p1, p2, d = (points_proches q) in
    let p_proche_t, dist_t = (plus_proche t q) in
    if dist_t < d then t, p_proche_t, dist_t
    else p1, p2, d;;

plus_proche (0., 0.) points;;
points_proches points;;