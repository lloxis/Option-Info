let verif f =
  let (t, p) = f in
  let n = Array.length t in
  let reponse = ref true in
  for i = 0 to n-1 do
    if t.(i) >= p
      then reponse := false done;
  !reponse;;

let est_croissante f =
  let (t, p) = f in
  let n = Array.length t in
  let reponse = ref true in
  for i = 1 to n-1 do
    if t.(i) < t.(i-1)
      then reponse := false done;
  !reponse;;

let composition (t1, p1) (t2, p2) =
  let n1 = Array.length t1 in
  let n2 = Array.length t2 in
  if p2 > n1 then failwith "il faut p2 <= n2 !!";
  let t_response = Array.make n2 0 in
  for i = 0 to n2-1 do
    t_response.(i) <- t1.(t2.(i)) done;
  (t_response, p2);;

(* ici il suffit de tester si il n'y a pas 2 fois le même nombre *)
let est_injective f =
  let response = ref true in
  let (t, p) = f in
  let n = Array.length t in
  let compteurs = Array.make p 0 in
  for i = 0 to n-1 do
    compteurs.(t.(i)) <- compteurs.(t.(i)) + 1 ;
    if compteurs.(t.(i)) > 1 then response := false done;
  !response;;

(* est_injective ([|1; 4; 5; 7; 7; 0|], 10);; *)

(* ici il suffit de tester si il y a au moins au chiffre de chaque (entre 0 et n-1) *)
let est_surjective f =
  let (t, p) = f in
  let response = ref true in
  let n = Array.length t in
  let compteurs = Array.make p 0 in
  for i = 0 to n-1 do
    compteurs.(t.(i)) <- compteurs.(t.(i)) + 1 done;
  for i = 0 to p-1 do
    if compteurs.(i) < 1 then response := false done;
  !response;;

est_surjective ([|1; 4; 2; 3; 6; 8; 9; 5; 7; 7; 0|], 10);;

let inverse f =
  let (t, p) = f in
  if not (est_surjective f && est_injective f) then failwith "f n'est pas bijective";
  let response = Array.make p 0 in
  for i = 0 to p-1 do
    response.(t.(i)) <- i done;
  response, p;;


let array_inverse = inverse ([|1; 4; 2; 3; 6; 8; 9; 5; 7; 0|], 10);;
composition ([|1; 4; 2; 3; 6; 8; 9; 5; 7; 0|], 10) array_inverse;;

(* pas fini *)
let speudo_inverse f =
  let (t, p) = f in
  let n = Array.length t in
  if est_surjective f then begin
    let response = Array.make p 0 in
    for i = 0 to n-1 do
      response.(t.(i)) <- i done;
  response, p end
  else t, p;;

let array_speudo_inverse = speudo_inverse ([|1; 4; 5; 7; 0|], 10);;
