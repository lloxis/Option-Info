type 'a arbre = F | Noeud of 'a arbre * 'a * 'a arbre;;

let a = Noeud(Noeud(Noeud(F, 4, F),
                    15,
                    Noeud(Noeud(F, 5, F), 8, Noeud(F, 3, F))),
              11,
              Noeud(Noeud(F, 7, F),
                    1,
                    Noeud(Noeud(F, 12, F), 13, F)));;

let profondeur arbr =
  let rec aux ar p =
    match ar with
    |F -> F
    |Noeud(g, r, d) -> Noeud(aux g (p+1), (r, p), aux d (p+1))
  in
  aux arbr 0;;

profondeur a;;

let rec infixe arbr =
  match arbr with
  |F -> ()
  |Noeud(g, r, d) -> infixe g;
                    print_int r;
                    print_newline;
                    infixe d;;

infixe a;;

(* exo10 *)

let rec arbr_to_listes_infixe arbr =
  let out1= ref [] in
  match arbr with
  |F -> []
  |Noeud(g, r, d) -> (arbr_to_listes g)@(r::(arbr_to_listes d));;

arbr_to_listes a;;