(* exo 1 *)
let f x = x**2.;;
let calculer_u n u0 f =
  let resultat = ref (u0 +. 0.) in
  for i = 1 to n do
      resultat := f !resultat done;
  !resultat;;
calculer_u 5 2. f;;

(* exo 2 *)
