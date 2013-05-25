(* pretty sure there is a better recursive way *)
(* Problem #1 *)
fun is_older(date1 : int*int*int, date2 : int*int*int) = 
  if #1 date1 < #1 date2
  then true
  else 
    if #1 date1 = #1 date2
    then
      if #2 date1 < #2 date2
      then true
      else
        if #2 date1 = #2 date2
        then
          if #3 date1 < #3 date2
          then true
          else false
        else false
    else false

(* PROBLEM #2 *)
fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month(tl dates, month)
  else 0 + number_in_month(tl dates, month)

(* PROBLEM #3 *)
fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* PROBLEM #4 *)
fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else
    if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* PROBLEM #5 *)
fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* PROBLEM #6 *)
fun get_nth(strings : string list, n : int) =
  if n=1
  then hd strings
  else get_nth(tl strings, n-1)

(* PROBLEM #7 *)
fun date_to_string(date : (int * int * int) ) =
  let val months = ["January", "February", "March",
                    "April","May", "June", "July", "August",
                    "September", "October","November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* PROBLEM #8 *)
fun number_before_reaching_sum (sum : int, nums : int list) = 
  if sum - hd nums > 0
  then 1 + number_before_reaching_sum(sum - hd nums, tl nums)
  else 0

(* PROBLEM #9 *)
fun what_month(day : int) =
  let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    1+number_before_reaching_sum(day, months)
  end

(* PROBLEM #10 *)
fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)

(* PROBLEM #11 *)
fun oldest( dates : (int * int * int) list ) =
  if null dates
  then NONE
  else
    let val tl_ans = oldest(tl dates)
    in
      if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
      then tl_ans
      else SOME (hd dates)
    end
