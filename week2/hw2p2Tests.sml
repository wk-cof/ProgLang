(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
(*
fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end
    *)
val test1a1 = all_except_option("1", ["1","4","6","1","5"]) = SOME ["4","6","5"];
val test1a2 = all_except_option("1", ["2","3","4","1","5"]) = SOME ["2","3","4","5"];
val test1a3 = all_except_option("1", []) = NONE;

val test1b1 =
 get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
 =["Fredrick","Freddie","F"];
val test1c1 =
 get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
 =["Fredrick","Freddie","F"];

val test1d1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"}) =
[{first="Fred", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="Fredrick", last="Smith", middle="W"}];

val nineOfDiamonds = (Diamonds,Num(9));
val queenOfSpades = (Spades,Queen);
val jackOfClubs   = (Clubs,Jack);
val test2a1 = card_color(nineOfDiamonds) = Red;
val test2a2 = card_color(queenOfSpades) = Black;
val test2a3 = card_color(jackOfClubs) = Black;


val test2b1 = card_value(jackOfClubs) = 10;
val test2b2 = card_value(nineOfDiamonds) = 9;

val test2c1 = remove_card([nineOfDiamonds, queenOfSpades, jackOfClubs],
  queenOfSpades, IllegalMove) = [nineOfDiamonds, jackOfClubs];
val test2c1 = remove_card([nineOfDiamonds, queenOfSpades, jackOfClubs],
  jackOfClubs, IllegalMove) = [nineOfDiamonds, queenOfSpades];
  
val test2d1 = all_same_color([nineOfDiamonds, queenOfSpades, jackOfClubs]) =
  false;

val test2d2 = all_same_color([queenOfSpades, jackOfClubs]) =
  true;
val test2e1 = sum_cards([nineOfDiamonds, jackOfClubs]) = 19;
val test2e2 = sum_cards([queenOfSpades, jackOfClubs]) = 20;

val test2f1 = score([nineOfDiamonds, jackOfClubs, queenOfSpades], 20) = 27;
val test2f1 = score([jackOfClubs, queenOfSpades], 30) = 5;
