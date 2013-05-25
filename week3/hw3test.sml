val tp0 = Wildcard;
val tp1 = TupleP ([Wildcard, UnitP, Variable ("Bhu"), Wildcard, ConstructorP ("Wuz", TupleP ([Variable ("Here"), Wildcard])), Variable ("")]);
val tp2 = TupleP ([ConstP(2), UnitP, Variable ("Bhu"), ConstP(7), ConstructorP ("Wuz", TupleP ([Variable ("Here"), UnitP])), Variable ("Bhu")]);
val tp3 = ConstructorP ("Combo", TupleP ([tp0, Wildcard, tp1, ConstP (84), tp2]));

fun check_against_default B A' A =
	if A = A' 
	then SOME B
	else NONE
	
val hw3Q1_1 = only_capitals ["Qwer", "qwer", "qWer", "t", "Y"] = ["Qwer", "Y"];

val hw3Q2_1 = longest_string1 [] = "";
val hw3Q2_2 = longest_string1 ["a"] = "a";
val hw3Q2_3 = longest_string1 ["a", "aa"] = "aa";
val hw3Q2_4 = longest_string1 ["a", "aa", "aaa", "aa", "aab"] = "aaa";

val hw3Q3_1 = longest_string2 ["a", "aa", "aaa", "aa", "aab"] = "aab";

val hw3Q4_1 = longest_string3 ["a", "aa", "aaa", "aa", "aab"] = "aaa";
val hw3Q4_2 = longest_string4 ["a", "aa", "aaa", "aa", "aab"] = "aab";

val hw3Q5_1 = longest_capitalized ["a", "Aa", "aAaa", "aa", "aab"] = "Aa";

val test6a = rev_string "BhuWuzHere" = "ereHzuWuhB";
val test6b = rev_string "" = "";

val test7a = (first_answer (check_against_default 27 "Bhu") ["Ac", "aBCd", "bhu", "Wuz", "hERE"] handle NoAnswer => 84) = 84;
val test7b = (first_answer (check_against_default 27 "bhu") ["Ac", "aBCd", "bhu", "Wuz", "hERE"] handle NoAnswer => 84) = 27;
val test7c = (first_answer (check_against_default "WooHoo!!" 512) []    handle NoAnswer => "D'oh") = "D'oh";
val test7d = (first_answer (check_against_default "WooHoo!!" 512) [512] handle NoAnswer => "D'oh") = "WooHoo!!";

val test8a = (all_answers (check_against_default [27] "bhu") ["Ac", "aBCd", "bhu", "Wuz", "hERE"]) = NONE;
val test8b = (all_answers (check_against_default [27] "bhu") ["bhu"]) = SOME [27];
val test8c = (all_answers (check_against_default [27] "bhu") []) = SOME [];

val hw9a_a = count_wildcards (tp0) = 1;
val hw9a_b = count_wildcards (tp1) = 3;
val hw9a_c = count_wildcards (tp2) = 0;
val hw9a_d = count_wildcards (tp3) = 5;

val hw3Q9b_1 = count_wild_and_variable_lengths (tp0) = 1;
val hw3Q9b_2 = count_wild_and_variable_lengths (tp1) = 10;
val hw3Q9b_3 = count_wild_and_variable_lengths (tp2) = 10;
val hw3Q9b_4 = count_wild_and_variable_lengths (tp3) = 22;

val hw3Q9c_1 = count_some_var ("Bhu", tp0) = 0;
val hw3Q9c_2 = count_some_var ("Bhu", tp1) = 1;
val hw3Q9c_3 = count_some_var ("Bhu", tp2) = 2;
val hw3Q9c_4 = count_some_var ("Bhu", tp3) = 3;
val hw3Q9c_5 = count_some_var ("Combo", tp3) = 0;

val hw3Q10a_1 = check_pat (tp0) = true;
val hw3Q10a_2 = check_pat (tp1) = true;
val hw3Q10a_3 = check_pat (tp2) = false;
val hw3Q10a_4 = check_pat (tp3) = false;
