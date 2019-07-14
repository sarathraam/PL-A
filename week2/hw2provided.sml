(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

	     
(* a *)
fun all_except_option (e, lst) =
    case lst of
	[] => NONE
      | (h::t) => let val matched = same_string(e, h) in
		      case all_except_option(e, t) of
			       NONE => if matched then SOME t else NONE
			     | SOME xs => SOME (if matched then xs else h::xs)
		       end
					   
					   
(* b *)
fun get_substitutions1 (lls, s) =
    case lls of
	[] => []
      | (f::r) => let val tl_ans = get_substitutions1(r, s) in
			 case all_except_option(s, f) of
			     NONE => tl_ans
			  |  SOME ls => ls @ tl_ans
		     end

			 
(* c *)
fun get_substitutions2 (lls, s) =
    let
	fun helper (lls, acc) =
	    case lls of
		[] => acc
	      | (f::r) => case all_except_option(s, f) of
			      NONE => helper(r, acc)
			    | SOME ls => helper(r, ls@acc)
    in
	helper(lls, [])
    end

	
(* d *)
fun similar_names (lls, nr) =
    let
	fun combinations ln =
	    case (ln, nr) of
		([], _) => []
	      | ((h::t), {first, middle, last}) => {first = h, middle = middle, last = last} :: combinations(t)

	(* short hand - could bind this to a variable like first=f *)
	val {first, middle, last} = nr
    in
	(* append first so the original record is included in the result as well *)
	combinations(first::get_substitutions1(lls, first))
    end    
	
	     
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
		       
datatype color = Red | Black
datatype move = Discard of card | Draw 
				      
exception IllegalMove


(* a *)
fun card_color (s, _) =
    case s of
	(Clubs | Spades) => Black 
      | (Diamonds | Hearts) => Red 


(* b *)
fun card_value (_, r) =
    case r of
	Num x => x
      | Ace => 11
      | _ => 10 


(* c *)
fun remove_card (cs, c, e) = let
    fun rc cs =
	case cs of
	    [] => raise e
	  | (x::t) => if x=c then t else x::rc(t) 
in
    rc(cs)
end


(* d *)
fun all_same_color cs =
    case cs of
	([] | _::[]) => true
      | (h::n::t) => card_color(h)=card_color(n) andalso all_same_color(n::t)


(* e *)
fun sum_cards cs = let
    fun sc (cs, acc) =
	case cs of
	    [] => acc
	  | (x::t) => sc(t, acc + card_value x)
in
    sc(cs, 0)
end


(* f *)
fun score (cs, g) = let
    val sum = sum_cards cs
    val same_color = all_same_color cs
    val prelim_score = if sum > g then 3 * (sum - g) else (g - sum)
in
    if same_color then prelim_score div 2 else prelim_score
end
			

(* g *)
fun officiate (cards, moves, goal) =
    let
	fun sim (held, cards, moves) =
	    case (cards, moves) of
		((_, []) | ([], _)) => held (* stop when one of the lists is done *)
	      | ((c::cs), (m::ms)) =>
		case m of
		    Discard c => let val new_held = remove_card(held, c, IllegalMove)
				 in
				     sim(new_held, cards, ms)
				 end
		  | Draw => let val new_held = c::held
			    in
				if sum_cards(new_held) < goal
				then sim(new_held, cs, ms)
				else new_held
			    end									in
	score(sim([], cards, moves), goal)
    end


(* 3a *)
fun score_challenge (cs, g) =
    let
	fun sum_sub (cs, acc) =
	    case cs of
		[] => acc
	      | ((_, cs)::t) => sum_sub(t, if cs=Ace andalso (acc - 10) > g
					   then acc - 10
					   else acc)
	val sum = sum_sub(cs, sum_cards cs)
	val prelim = if sum > g then 3 * (sum - g) else (g - sum)
	val same_color = all_same_color cs
    in
	if same_color then prelim div 2 else prelim
    end


fun officiate_challenge (cards, moves, goal) =
    let
	fun sim (held, cards, moves) =
	    case (cards, moves) of
		((_, []) | ([], _)) => held (* stop when one of the lists is done *)
	      | ((c::cs), (m::ms)) =>
		case m of
		    Discard c => let val new_held = remove_card(held, c, IllegalMove)
				 in
				     sim(new_held, cards, ms)
				 end
		  | Draw => let val new_held = c::held
			    in
				if sum_cards(new_held) < goal
				then sim(new_held, cs, ms)
				else new_held
			    end									in
	score_challenge(sim([], cards, moves), goal)
    end
	
