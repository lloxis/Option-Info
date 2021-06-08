let crible n =
  let t = Array.make (n+1) false in
  for i=2 to n/2 do
    for k=2 to n/i do
      t.(i*k) <- true;
    done;
  done;
  t;;

crible 100;;


let coef_binom2 k n =
  let t = Array.make n 0 in
  t.(0) <- 1;
  for i=1 to n-1 do
    for j=i downto 1 do
      t.(j) <- t.(j) + t.(j-1);
    done;
  done;
  t.(k+1);;

coef_binom2 5 20;;


(*

let mat_nulle n p = Array.make n (Array.make p 0);;

let identite n =
  let t = mat_nulle n n in
  for i=0 to n-1 do
    t.(i).(i) <- 1;
  done;
  t;;

let somme_mat a b =
  let n = Array.length a in
  let p = Array.length a.(0) in
  let t = mat_nulle n p in
  for i=0 to n-1 do
    for j=0 to p-1 do
      t.(i).(j) <- a.(i).(j) + b.(i).(j);
    done;
  done;
  t;;
 *)
