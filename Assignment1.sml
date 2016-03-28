fun is_older(date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) <> (#1 date2)
    then (#1 date1) < (#1 date2)
    else if(#2 date1) <> (#2 date2)
        then (#2 date1 < #2 date2)
        else #3 date1 < #3 date2
(*val test = is_older((1991,12,21), (1991,11,21))*)

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
        then 1 + number_in_month((tl dates), month)
        else number_in_month((tl dates), month)

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates: (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
        then hd dates :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else let fun append(x: (int*int*int) list, y : (int*int*int) list) =
        if null x
        then y
        else hd x :: append (tl x, y)
        in append (dates_in_month(dates, hd months), dates_in_months(dates, tl months))
        end
(*val test = dates_in_months([(1,1,1),(1,2,1),(2,1,1)], [1, 2])*)

fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)
(*val test = get_nth(["first", "second","third"], 2)*)

fun date_to_string(date: int*int*int) =
    let val month_list =["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(month_list, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end
(*val test = date_to_string((1993,11,21))*)

fun number_before_reaching_sum(ints : int list, sum : int) =
    if hd ints < sum
    then 1 + number_before_reaching_sum(tl ints, sum - hd ints)
    else 0
(*val test = number_before_reaching_sum([2,3,4,5], 6)*)

fun what_month(day : int) =
    let val day_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31 ,30, 31]
    in number_before_reaching_sum(day_list, day) + 1
    end
(*val test = what_month(59)*)

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else let fun count(from: int, to: int) =
        if from = to
        then to :: []
        else
            from :: count(from + 1, to)
        in count(what_month(day1), what_month(day2))
        end
(*val test = month_range(32, 365)*)

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else let fun old(date: int*int*int, date_list : (int*int*int) list) =
        if null date_list
        then SOME date
        else if is_older(date, hd date_list)
            then old(date, tl date_list)
            else old(hd date_list, tl date_list)
            in old(hd dates, tl dates)
            end
(*val test = oldest([(1992,11,5), (1842,6,30), (1934,7,14)])*)

fun remove_duplicate(dup_list : int list) =
    let fun contain(res_list: int list, num : int) =
        if null res_list
        then false
        else if num = hd res_list
            then true
            else contain(tl res_list, num)
    in
        if null dup_list
        then []
        else if contain(tl dup_list, hd dup_list)
            then remove_duplicate(tl dup_list)
            else hd dup_list :: remove_duplicate(tl dup_list)
    end

fun number_in_months_challenge (dates: (int*int*int) list, months : int list) =
    let val non_duplicate_months = remove_duplicate(months) in number_in_months(dates, non_duplicate_months) end

fun dates_in_months_challenge (dates: (int*int*int) list, months : int list) =
    let val non_duplicate_months = remove_duplicate(months) in dates_in_months(dates, non_duplicate_months) end


fun reasonable_date (date: int*int*int) =
    if (#1 date) < 0 orelse (#2 date) <= 0 orelse (#3 date <= 0) orelse (#3 date) > 12
    then false
    else let fun get_days(days_list : int list, month : int) =
        if month = 1
        then hd days_list
        else get_days(tl days_list, month - 1)
        in
            if (#1 date) mod 400 = 0 orelse ((#1 date) mod 4 = 0 andalso (#1 date) mod 100 > 0)
            then get_days([31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], (#2 date)) >= (#3 date)
            else get_days([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], (#2 date)) >= (#3 date)
        end



