-module(calex_TEST).
-export([test/0, test/1]).
-import(calex,
       [	
	tomorrow/0,
         tomorrow/1,
         yesterday/0,
         yesterday/1,
         next_year/0,
         next_year/1,
         last_year/0,
         last_year/1,
	 last_month/0,
	 last_month/1,
	 next_month/0,
	 next_month/1,
	 start_of_the_month/0,
	 start_of_the_month/1,
         start_of_the_week/0,
         start_of_the_week/1,
         start_of_the_day/0,
         start_of_the_day/1,
         start_of_the_hour/0,
         start_of_the_hour/1,
         start_of_the_minute/0,
         start_of_the_minute/1,
         middle_of_the_day/0,
         middle_of_the_day/1,
	 end_of_the_month/0,
	 end_of_the_month/1,
         end_of_the_week/0,
         end_of_the_week/1,
         end_of_the_day/0,
         end_of_the_day/1,
         end_of_the_hour/0,
         end_of_the_hour/1,
         end_of_the_minute/0,
         end_of_the_minute/1,
	 seconds_since_midnight/0,
 	 seconds_since_midnight/1,
	 seconds_until_end_of_day/0,
	 seconds_until_end_of_day/1,
	 dayname_of_the_week/0,
         dayname_of_the_week/1,
	 month_name/0,
	 month_name/1
       ]
       ).

test(all) ->
	DateTime = {{2023, 02, 23}, {18, 31, 01}},
	test_individual(DateTime, true).
test() ->
        DateTime = {{2023, 02, 23}, {18, 31, 01}},
	test_individual(DateTime, false).

test_individual(DateTime, All) ->
	io:format("TEST Against Date Time ~p~n", [DateTime]),
	{Date, Time} = DateTime,
	{Hour, Minute, _} = Time,
	TestResults = 
	eval(tomorrow(DateTime), {2023,02,24}, tomorrow) ++
        eval(yesterday(DateTime), {2023, 02, 22}, yesterday) ++
	eval(next_year(DateTime), {{2024, 02, 23}, Time}, next_year) ++
	eval(last_year(DateTime), {{2022,02,23},Time}, last_year) ++
	eval(next_month(DateTime), {{2023, 03, 23}, Time}, next_month) ++
	eval(last_month(DateTime), {{2023, 01, 23}, Time}, last_month) ++
	eval(end_of_the_month({{2020, 02, 15}, {18, 31, 45}}), {{2020, 02, 29}, {23,59,59}}, 'end_of_the_month leap_year') ++
	eval(end_of_the_month(DateTime), {{2023, 02, 28}, {23,59,59}}, end_of_the_month) ++
	eval(end_of_the_week(DateTime), {{2023, 02, 26},{23,59,59}}, end_of_the_week) ++
	eval(end_of_the_day(DateTime), {Date, {23,59,59}}, end_of_the_day) ++
	eval(end_of_the_hour(DateTime), {Date, {Hour, 59, 59}},end_of_the_hour) ++
	eval(end_of_the_minute(DateTime), {Date, {Hour, Minute, 59}}, end_of_the_minute) ++
	eval(start_of_the_month(DateTime), {{2023, 02, 01}, {0,0,0}}, start_of_the_month) ++
	eval(start_of_the_week(DateTime), {{2023, 02, 19}, {0,0,0}}, start_of_the_week) ++
	eval(start_of_the_day(DateTime), {Date, {0,0,0}}, start_of_the_day) ++
	eval(start_of_the_hour(DateTime), {Date, {Hour,0, 0}}, start_of_the_hour) ++
	eval(start_of_the_minute(DateTime), {Date, {Hour, Minute,0}}, start_of_the_minute) ++
	eval(seconds_since_midnight(DateTime), 66661, seconds_since_midnight) ++
	eval(seconds_until_end_of_day(DateTime), 19739, seconds_until_end_of_day)++
	eval(dayname_of_the_week(DateTime), {4, 'Thursday'}, 'dayname_of_the_week by datetime')++
	eval(dayname_of_the_week(1), {1, 'Monday'}, 'dayname_of_the_week')++
	eval(dayname_of_the_week(2), {2, 'Tuesday'}, 'dayname_of_the_week')++
	eval(dayname_of_the_week(3), {3, 'Wednesday'}, 'dayname_of_the_week')++
	eval(dayname_of_the_week(4), {4, 'Thursday'}, 'dayname_of_the_week')++
	eval(dayname_of_the_week(5), {5, 'Friday'}, 'dayname_of_the_week')++
	eval(dayname_of_the_week(6), {6, 'Saturday'}, 'dayname_of_the_week')++
	eval(dayname_of_the_week(7), {7, 'Sunday'}, 'dayname_of_the_week') ++
	eval(month_name(DateTime), {2, 'February'}, 'month_name by datetime')++
	eval(month_name(1), {1, 'January'}, month_name)++
	eval(month_name(2), {2, 'February'}, month_name)++
	eval(month_name(3), {3, 'March'}, month_name)++
	eval(month_name(4), {4, 'April'}, month_name)++
	eval(month_name(5), {5, 'May'}, month_name)++
	eval(month_name(6), {6, 'June'}, month_name)++
	eval(month_name(7), {7, 'July'}, month_name)++
	eval(month_name(8), {8, 'August'}, month_name)++
	eval(month_name(9), {9, 'September'}, month_name)++
	eval(month_name(10), {10, 'October'}, month_name)++
	eval(month_name(11), {11, 'November'}, month_name)++
	eval(month_name(12), {12, 'December'}, month_name)++
	eval(month_name(13), {13, 'not_found'}, month_name)
,

	if All =:= true ->	
		Results = [{if T -> pass; true -> 'FAIL' end, G} || {T, G} <- TestResults];
	 true ->
		Results = [{if T -> pass; true -> 'FAIL' end, G} || {T, G} <- TestResults, T =:= false]
	end,

	if 
		All =:= true ->
		   io:format("~p~n", [Results]),
		   "All tests passed. Use calex_TEST:test(all) for complete result";
		length(Results) /= 0 ->
		   io:format("~p~n", [Results]);
		true ->
		   "All tests passed. Use calex_TEST:test(all) for complete result"
	end.

eval(LeftPredicate, RightPredicate, FunctionName) ->
	[
	 	{
		 LeftPredicate =:= RightPredicate,
		 [ 
		  	FunctionName, 
			LeftPredicate, 
			RightPredicate
		 ]
		}
	].
