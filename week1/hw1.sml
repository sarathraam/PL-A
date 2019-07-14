fun is_older (first : (int * int * int), second : (int * int * int)) =
    if #1 second > #1 first
    then true
    else
	if #1 second < #1 first
	then false
	else
	    if #2 second > #2 first
	    then true
	    else
		if #2 second = #2 first
		then #3 second > #3 first
		else false

									     
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else 
	let
	    val count_me = if (#2 (hd dates)) = month then 1 else 0;
	in
	    count_me + number_in_month(tl dates, month)
	end

	    
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	if #2 (hd dates) = month
	then hd dates :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)


fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth (strings : string list, n : int) =
    let
	fun get_at (strings : string list, i : int) =
	    if n = i
	    then hd strings
	    else get_at(tl strings, i + 1)
    in
	get_at(strings, 1)
    end

	
fun date_to_string (date : (int * int * int)) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum (sum : int, values : int list) =
    let
	fun search (values : int list, current_sum : int,  n : int) =
	    if null values orelse hd values + current_sum >= sum
	    then n
	    else search(tl values, hd values + current_sum, n + 1)
    in
	search(values, 0, 0)
    end


fun what_month (day : int) =
    let
	val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    in
	number_before_reaching_sum(day, days_of_months) + 1
    end


fun month_range (from : int, to : int) =
    if from > to
    then []
    else what_month(from) :: month_range(from + 1, to)


fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	if null (tl dates)
	then SOME (hd dates)
	else
	    let
		val tl_oldest = oldest(tl dates);
	    in
		if isSome tl_oldest andalso is_older(hd dates, valOf tl_oldest)
		then SOME (hd dates)
		else tl_oldest
	    end


fun has_item (values : int list, item : int) =
    if null values
    then false
    else
	if hd values = item then true
	else has_item(tl values, item)


fun unique_list (values : int list) =
    if null values
    then []
    else
	if null (tl values)
	then hd values :: []
	else
	    let
		val new_list = unique_list(tl values);
	    in
		if has_item(new_list, hd values)
		then new_list
		else hd values :: new_list
	    end
		
		
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let
	val without_duplicates = unique_list(months);
    in
	number_in_months(dates, without_duplicates)
    end


fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let
	val without_duplicates = unique_list(months);
    in
	dates_in_months(dates, without_duplicates)
    end


fun is_leap_year (year : int) =
    year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0


fun get_nth_int (values : int list, n : int) =
    let
	fun get_at (values_ : int list, i : int) =
	    if null values_
	    then NONE
	    else
		if n = i
		then SOME (hd values_)
		else get_at(tl values_, i + 1)
    in
	get_at(values, 1)
    end
			   
								       
fun reasonable_date (date : (int * int * int)) =
    if #1 date <= 0
    then false
    else
	let
	    val days_in_month = get_nth_int([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], #2 date);
	in
	    if isSome days_in_month
	    then
		let
		    val days_adjusted = valOf days_in_month + (if #2 date = 2 andalso is_leap_year(#1 date) then 1 else 0);
		in
		    #3 date > 0 andalso #3 date <= days_adjusted	    	     
		end
	    else false
	end
		    
		    
		
	    
	    
	
