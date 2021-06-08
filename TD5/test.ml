let rec heron n =
  match n with
  |0 -> 1.
  |_ -> 0.5*.(1.+.2./.(heron (n-1)))