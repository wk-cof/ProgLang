(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun rev xs =
  let fun aux(xs, acc) =
    case xs of
       [] => acc
     | x::xs' => aux(xs',x::acc)
  in
    aux(xs,[])
  end

(* put your solutions for problem 1 here *)

(* PROBLEM #1 A *)
fun all_except_option(a, bs) =
  case bs of
       [] => NONE
     | _  =>
        let
          fun all_except_helper(a, bs, cs) = (* should be rail-recursive*)
          case bs of
             [] => cs
           | (b::bs') => if same_string(a,b) 
                         then all_except_helper(a, bs', cs)
                         else all_except_helper(a, bs', b::cs)
        in
          SOME(rev(all_except_helper(a, bs, [])))
        end


(* PROBLEM #1 B *)
fun get_substitutions1(ass, b) =
  case ass of
       []       => []
     | (a::ass') => let 
                      val temp = all_except_option(b, a)
                    in 
                      if isSome temp andalso valOf temp <> a
                      then valOf temp @ get_substitutions1(ass', b)
                      else get_substitutions1(ass', b)
                    end

(* PROBLEM #1 C *)
fun get_substitutions2(ass, b) =
  let
  fun get_substitution_helper(ass, b, cs) =
    case ass of
         [] => cs
       | (a::ass') =>let
                        val temp = all_except_option(b,a)
                     in
                        if isSome temp
                        then  if valOf temp = a
                              then get_substitution_helper(ass', b, cs)
                              else get_substitution_helper(ass', b, cs @ valOf temp )
                        else
                          get_substitution_helper(ass', b, cs)
                     end
  in
    get_substitution_helper(ass, b, [])
  end

(* PROBLEM #1 D *)

fun similar_names(name_lists, full_name) =
  let val {first=x, middle=y, last=z} = full_name
      val sub_list = get_substitutions2(name_lists, x)
    fun my_helper (fnames) =
      case fnames of
           []                 => [{first=x,middle=y,last=z}]
         | (fname::fnames') => {first=fname, middle=y, last=z} :: my_helper(fnames')
  in
    rev(my_helper(sub_list))
  end


(**************************************** PART 2 *****************************************)
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (s,r) =
  case s of
       Clubs => Black
     | Spades => Black
     | _ => Red

fun card_value(s, r) =
  case r of
       Ace => 11
     | Num(x) => x
     | _   => 10

fun remove_card(cs, c, e) =
  case cs of
       [] => raise e
     | (top::cs') => if top = c
                     then cs'
                     else top::remove_card(cs', c, e)

fun all_same_color(cards) =
  case cards of
       []           => true
     | (c::cards')  => case cards' of
                              []            => true
                            | (c2::cards'') => (card_color(c) = card_color(c2)) andalso all_same_color(cards')

fun sum_cards(cards) =
  let fun sum_cards_helper(cards, cur_sum) =
    case cards of
         [] => cur_sum
       | (c::cards') => sum_cards_helper(cards', card_value(c)+cur_sum)
  in
    sum_cards_helper(cards, 0)
  end

fun score (cards, goal) =
  let val s_cards = sum_cards(cards)
  fun pre_score() =
    if (s_cards > goal)
    then (s_cards - goal)*3
    else (goal - s_cards)
  in
    if all_same_color(cards)
    then pre_score() div 2
    else pre_score()
  end
