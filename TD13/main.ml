type ('a, 'b) table = {mutable contenu : ('a*'b) list };;

let t1 = {contenu= [(1, "a"); (2, "b"); (3, "c")]};;

let t2 = {contenu= []};;
for k = 20 downto 1 do
  t2.contenu <- (k, k*k)::(t2.contenu) done;;

let table_vide = {contenu= []};;

let test_table_vide t = t=table_vide;;

let ajouter t c = {contenu= c::t.contenu};;

let rec test_clef t clef =
  match t.contenu with
  |[] -> false
  |(c, _)::q -> c=clef || (test_clef {contenu=q} clef);;

test_clef t1 1;;
test_clef t1 0;;

let rec valeur t clef =
  match t.contenu with
  |[] -> failwith "clef non trouvée"
  |(c, v)::q when c=clef -> v
  |t::q -> valeur {contenu=q} clef;;

valeur t1 0;;
valeur t1 1;;
valeur t2 3;;


let rec supprime t clef =
  match t.contenu with
  |[] -> table_vide
  |(c, v)::q when c=clef -> {contenu= q}
  |t::q -> {contenu= t::((supprime {contenu=q} clef).contenu)};;

let rec table_to_liste t =
  match t.contenu with
  |[] -> table_vide
  |(c, v)::q -> v::(table_to_liste {contenu=q});;

table_to_liste t2;;

(* let rec listes_to_table clefs valeurs =
  match clefs, valeurs with
  |[], [] -> table_vide
  |t1::q1, t2::q2 -> {contenu= (t1, t2)::(listes_to_table q1 q2)};; *)

