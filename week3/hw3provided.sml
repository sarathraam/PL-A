(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

			       
(** Solutions **)

(* 1 *)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs


(* 2 *)
fun longest_string1 xs =
    List.foldl (fn (x, m) => if String.size x > String.size m then x else m) "" xs


(* 3 *)
fun longest_string2 xs =
    List.foldl (fn (x, m) => if String.size x >= String.size m then x else m) "" xs


(* 4 *)
fun longest_string_helper f =
    (fn xs => List.foldl (fn (x, m) => if f(String.size x, String.size m) then x else m) "" xs)


(* 4a *)
val longest_string3 = longest_string_helper (fn (x, y) => x > y)

				     
(* 4b *)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)
		   
		   
(* 5 *)
val longest_capitalized = longest_string1 o only_capitals


(* 6 *)
val rev_string = String.implode o List.rev o String.explode
    

(* 7 *)
fun first_answer f =
    (fn xs =>
	case (List.foldl (fn (x, a) => if isSome a then a else f(x)) NONE xs) of
	    NONE => raise NoAnswer
	  | SOME v => v)


(* 8 *)
fun all_answers f =
    List.foldr (fn (x, a) =>
		   case a of
		       SOME l => (case f(x) of
				      SOME v => SOME(v@l)
				    | NONE => NONE)
		     | NONE => NONE)
	       (SOME [])
	       
	       
(* 9a *)
val count_wildcards = g (fn x => 1) (fn x => 0)
			

(* 9b *)
val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size x)
					
					
(* 9c *)
fun count_some_var (m, p) =
    g (fn x => 0) (fn x => if x = m then 1 else 0) p 	       


(* 10 *)
fun check_pat p = let

    fun collect_names ps =
	case ps of
	    Variable v => v::[]
	  | TupleP p => List.foldl (fn (x, a) => collect_names(x) @ a) [] p
	  | ConstructorP (_, p)  => collect_names p 
	  | _ => [] 
	

    fun distinct ns =
	case ns of
	    [] => true
	  | x::xs => not (List.exists (fn y => x = y) xs) andalso distinct xs 
									   
in
    (distinct o collect_names) p
end


(* 11 *)
fun match (v, p) =
    case (v, p) of
	(Const x, ConstP y) => if x = y then SOME [] else NONE
      | (v, Variable s) => SOME((s, v)::[])
      | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 then match(v, p) else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Unit, UnitP) => SOME []
      | (_, Wildcard) => SOME []
      |  _ => NONE


(* 12 *)
fun first_match v ps =
   SOME (first_answer match (List.map (fn x => (v, x)) ps)) handle NoAnswer => NONE
