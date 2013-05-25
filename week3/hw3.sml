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

 (* PROBLEM 1 *)
fun only_capitals strs =
  List.filter (fn str => Char.isUpper(String.sub(str,0))) strs

 (* PROBLEM 2 *)
fun longest_string1 strs =
  List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" strs

 (* PROBLEM 3 *)
fun longest_string2 strs =
  List.foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" strs

 (* PROBLEM 4 *)
(* fun longest_string_helper f strs =
  List.foldl (fn (x,y) => if f(x,y) then x else y) "" strs
  can't do this, doesn't match signature *)
fun longest_string_helper test_fn sl =
  let
  fun fold (f, acc, xs) =
     case xs of
         []    => acc
       | x::xs => fold (f, if f(acc, x) then x else acc, xs)
  in
     fold ((fn (s1, s2) => test_fn (String.size s2, String.size s1)), "", sl)
  end

val longest_string3 = longest_string_helper (fn (x,y) => x >  y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

 (* PROBLEM 5 *)
val longest_capitalized = longest_string1 o only_capitals;

 (* PROBLEM 6 *)
val rev_string = String.implode o List.rev o String.explode;

 (* PROBLEM 7 *)
exception NoAnswer

fun first_answer f aas =
  case aas of
       []     => raise NoAnswer
     | a::aas' => case f a of
                  SOME v  => v
                |  _      => first_answer f aas'
 (* PROBLEM 8 *)
fun all_answers f aas =
  let fun all_answers_helper (f,aas,acc) =
    case aas of
         []       => acc
       | a::aas'  => case f a of
                     SOME v   => all_answers_helper (f,aas', v @ acc)
                  |  NONE     => raise NoAnswer
  in
    SOME(all_answers_helper(f, aas, [])) handle NoAnswer => NONE
  end
 (* PROBLEM 9 a *)
(* val count_wildcards = fn : pattern -> int *)
val count_wildcards = g (fn _ => 1) (fn y => 0)

(* PROBLEM 9 b *)
(* val count_wild_and_variable_lengths = fn : pattern -> int *)
val count_wild_and_variable_lengths = g (fn _ => 1) (String.size)

(* PROBLEM 9 c *)
(* val count_some_var = fn : string * pattern -> int *)
fun count_some_var (str, pat) = g (fn _ => 0) (fn y => if str = y then 1 else 0)  pat

(* PROBLEM 10 *)
(* val check_pat = fn : pattern -> bool *)
fun check_pat pat =
  let
    fun prob10_helper1 (p, acc) =
      case p of
         Variable x         => x :: acc
        | TupleP ps         => List.foldl prob10_helper1 acc ps
        | ConstructorP(_,p) => prob10_helper1 (p, acc)
        | _                 => acc

    fun prob10_helper2 (strs : string list) =
      case strs of 
           []         => true
         | str::strs'  => if List.exists (fn a => a = str) strs'
                          then false
                          else prob10_helper2 strs'
  in
    prob10_helper2(prob10_helper1(pat, []))
  end

(* PROBLEM 11 *)
(* val match = fn : valu * pattern -> (string * valu) list option *)
(* PROBLEM 11 *)
(* val first_match = fn : valu -> pattern list -> (string * valu) list option *)

