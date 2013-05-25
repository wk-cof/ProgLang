val a = (2012, 01, 01);
val b = (2013, 01, 01);
val c = (2012, 02, 01);
val d = (2013, 05, 22);
val e = (2012, 99, 99);
val f = (2012, 05, 05);
val g = (1012, 11, 30);


val problem2=0;
is_older(a,a) = false;
is_older(a,b) = true;
is_older(a,c) = true;
is_older(a,d) = true;
is_older(d,c) = false;
is_older(c,b) = true;


val listofdates = [a,b,c,d,e,f,g];
val problem2=0;
number_in_month(listofdates,1) = 2;
number_in_month(listofdates,2) = 1;
number_in_month(listofdates,3) = 0;

val problem3=0;
number_in_months(listofdates,[1,2,3,5]) = 5;

val problem4=0;
dates_in_month(listofdates, 1) = [a,b];
dates_in_month(listofdates, 3) = [];
dates_in_month(listofdates, 11) = [g];

val problem5=0;
dates_in_months(listofdates, [1,2,5]) = [a,b,c,d,f];
dates_in_months(listofdates, [3,4,6,7,8,9,10,12]) = [];

val problem7=0;
date_to_string(a) = "January 1, 2012";
date_to_string(g) = "November 30, 1012";

val problem8=0;
number_before_reaching_sum(10,[1,1,2,3,3,4]) = 4;
number_before_reaching_sum(10,[1,1,1,3,3,4]) = 5;

val problem9=0;
what_month(30) = 1;
what_month(32) = 2;
what_month(60) = 3;
what_month(330) = 11;

val problem10=0;
month_range(31, 32) = [1,2];

val problem11=0;
oldest(listofdates) = SOME(g);

