let rec zeroed l = 
	match l with
	[] -> false |
	0::t -> true |
	h::t -> zeroed t;;


let rec exponentialing x f s t =
	if t < 0.000001
	then s
	else exponentialing x  (f +. 1.0) (s +. t) (t *. (x /. f));;



let exponential x = 
	exponentialing x 1.0 0.0 1.0;;


