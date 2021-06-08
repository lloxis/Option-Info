let rec hanoi n a b c =
  match n with
  |1 -> [(a, b)]
  |_ -> (hanoi (n-1) a c b)@((a, b)::(hanoi (n-1) c b a));;

hanoi 3 "1" "2" "3";;