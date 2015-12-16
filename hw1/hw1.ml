(* Returns true if a is a subset of b *)
let rec subset a b = match a,b with
  | [], _          -> true  (* empty set is a subset of anything *)
  | _, []          -> false (* only empty set is a subset of itself *)
  | [a], [b]       -> a = b (* matches only if they are the same list *)
  | ha::ta, hb::tb -> if (if ha = hb then true else subset [ha] tb) then
			subset ta b else false
		      (* Tricky logic. If the heads are the same we check the tail a
                         against all of b (in case of dupes). Otherwise we make sure
                         that the head exists in the tail of b in the first place. If
                         it isn't then a is not a subset. Otherwise, we keep checking *)

(* Returns true if a is a proper subset of b
   No set is a proper_subset of itself *)					   
let proper_subset a b = subset a b && (not (subset b a))

(* Returns true if the sets are equal. By definition, two sets a and b
   are equal if a is a subset of b and b is a subset of a *)
let equal_sets a b = subset a b && subset b a

(* Returns set difference a - b, that is, the set of all members of a that
   are also not members of b *)
let rec set_diff a b = match a, b with
  | [], _          -> [] (* Empty set base case by definition *)
  | ha::ta, []     -> (ha::ta) (* a - [] = a by definition *)
  | ha::ta, hb::tb -> if subset [ha] b then set_diff ta b else
			ha::(set_diff ta b)
						
			
		     
		      
