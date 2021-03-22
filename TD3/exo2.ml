let test_prime n =
  if n < 2 then false
  else
  let resultat = ref true in
  for i = 2 to n-1 do
      if n mod i = 0 then resultat := false done;
  !resultat;;
test_prime 1;;
test_prime 2;;
test_prime 4;;
test_prime 7;;
test_prime 13;;

let somme_prime n =
  let compteur_nb_premiers = ref 0 in
  for i = 1 to n do
      if test_prime i then compteur_nb_premiers := !compteur_nb_premiers + 1 done;