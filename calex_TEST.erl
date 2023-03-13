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
	 month_name/1,
	 month_name/2,
	 month_sname/0,
	 month_sname/1,
	 month_sname/2
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
	assertEqual(tomorrow(DateTime), {2023,02,24}, tomorrow) ++
        assertEqual(yesterday(DateTime), {2023, 02, 22}, yesterday) ++
	assertEqual(next_year(DateTime), {{2024, 02, 23}, Time}, next_year) ++
	assertEqual(last_year(DateTime), {{2022,02,23},Time}, last_year) ++
	assertEqual(next_month(DateTime), {{2023, 03, 23}, Time}, next_month) ++
	assertEqual(last_month(DateTime), {{2023, 01, 23}, Time}, last_month) ++
	assertEqual(end_of_the_month({{2020, 02, 15}, {18, 31, 45}}), {{2020, 02, 29}, {23,59,59}}, 'end_of_the_month leap_year') ++
	assertEqual(end_of_the_month(DateTime), {{2023, 02, 28}, {23,59,59}}, end_of_the_month) ++
	assertEqual(end_of_the_week(DateTime), {{2023, 02, 26},{23,59,59}}, end_of_the_week) ++
	assertEqual(end_of_the_day(DateTime), {Date, {23,59,59}}, end_of_the_day) ++
	assertEqual(end_of_the_hour(DateTime), {Date, {Hour, 59, 59}},end_of_the_hour) ++
	assertEqual(end_of_the_minute(DateTime), {Date, {Hour, Minute, 59}}, end_of_the_minute) ++
	assertEqual(start_of_the_month(DateTime), {{2023, 02, 01}, {0,0,0}}, start_of_the_month) ++
	assertEqual(start_of_the_week(DateTime), {{2023, 02, 19}, {0,0,0}}, start_of_the_week) ++
	assertEqual(start_of_the_day(DateTime), {Date, {0,0,0}}, start_of_the_day) ++
	assertEqual(start_of_the_hour(DateTime), {Date, {Hour,0, 0}}, start_of_the_hour) ++
	assertEqual(start_of_the_minute(DateTime), {Date, {Hour, Minute,0}}, start_of_the_minute) ++
	assertEqual(seconds_since_midnight(DateTime), 66661, seconds_since_midnight) ++
	assertEqual(seconds_until_end_of_day(DateTime), 19739, seconds_until_end_of_day)++
	assertEqual(dayname_of_the_week(DateTime), {4, 'Thursday'}, 'dayname_of_the_week by datetime')++
	assertEqual(dayname_of_the_week(1), {1, 'Monday'}, 'dayname_of_the_week')++
	assertEqual(dayname_of_the_week(2), {2, 'Tuesday'}, 'dayname_of_the_week')++
	assertEqual(dayname_of_the_week(3), {3, 'Wednesday'}, 'dayname_of_the_week')++
	assertEqual(dayname_of_the_week(4), {4, 'Thursday'}, 'dayname_of_the_week')++
	assertEqual(dayname_of_the_week(5), {5, 'Friday'}, 'dayname_of_the_week')++
	assertEqual(dayname_of_the_week(6), {6, 'Saturday'}, 'dayname_of_the_week')++
	assertEqual(dayname_of_the_week(7), {7, 'Sunday'}, 'dayname_of_the_week') ++
	assertEqual(month_name(DateTime), {2, 'February', "February"}, 'month_name by datetime')++
	assertEqual(month_name(1), {1, 'January', "January"}, month_name)++
	assertEqual(month_name(2), {2, 'February', "February"}, month_name)++
	assertEqual(month_name(3), {3, 'March', "March"}, month_name)++
	assertEqual(month_name(4), {4, 'April', "April"}, month_name)++
	assertEqual(month_name(5), {5, 'May', "May"}, month_name)++
	assertEqual(month_name(6), {6, 'June', "June"}, month_name)++
	assertEqual(month_name(7), {7, 'July', "July"}, month_name)++
	assertEqual(month_name(8), {8, 'August', "August"}, month_name)++
	assertEqual(month_name(9), {9, 'September', "September"}, month_name)++
	assertEqual(month_name(10), {10, 'October', "October"}, month_sname)++
	assertEqual(month_name(11), {11, 'November', "November"}, month_sname)++
	assertEqual(month_name(12), {12, 'December', "December"}, month_sname)++
	assertEqual(month_name(13), {13, 'not_found', "not_found"}, month_sname)++
	assertEqual(month_name(DateTime, en), {2, 'February', "February"}, 'month_name by datetime')++
	assertEqual(month_name(1, en), {1, 'January', "January"}, month_name)++
	assertEqual(month_name(2, en), {2, 'February', "February"}, month_name)++
	assertEqual(month_name(3, en), {3, 'March', "March"}, month_name)++
	assertEqual(month_name(4, en), {4, 'April', "April"}, month_name)++
	assertEqual(month_name(5, en), {5, 'May', "May"}, month_name)++
	assertEqual(month_name(6, en), {6, 'June', "June"}, month_name)++
	assertEqual(month_name(7, en), {7, 'July', "July"}, month_name)++
	assertEqual(month_name(8, en), {8, 'August', "August"}, month_name)++
	assertEqual(month_name(9, en), {9, 'September', "September"}, month_name)++
	assertEqual(month_name(10, en), {10, 'October', "October"}, month_sname)++
	assertEqual(month_name(11, en), {11, 'November', "November"}, month_sname)++
	assertEqual(month_name(12, en), {12, 'December', "December"}, month_sname)++
	assertEqual(month_name(13, en), {13, 'not_found', "not_found"}, month_sname)++
	assertEqual(month_name(DateTime, tr), {2, 'Subat', "Subat"}, 'month_name by datetime')++
	assertEqual(month_name(1, tr), {1, 'Ocak', "Ocak"}, month_name)++
	assertEqual(month_name(2, tr), {2, 'Subat', "Subat"}, month_name)++
	assertEqual(month_name(3, tr), {3, 'Mart', "Mart"}, month_name)++
	assertEqual(month_name(4, tr), {4, 'Nisan', "Nisan"}, month_name)++
	assertEqual(month_name(5, tr), {5, 'Mayis', "Mayis"}, month_name)++
	assertEqual(month_name(6, tr), {6, 'Haziran', "Haziran"}, month_name)++
	assertEqual(month_name(7, tr), {7, 'Temmuz', "Temmuz"}, month_name)++
	assertEqual(month_name(8, tr), {8, 'Agustos', "Agustos"}, month_name)++
	assertEqual(month_name(9, tr), {9, 'Eylul', "Eylul"}, month_name)++
	assertEqual(month_name(10, tr), {10, 'Ekim', "Ekim"}, month_sname)++
	assertEqual(month_name(11, tr), {11, 'Kasim', "Kasim"}, month_sname)++
	assertEqual(month_name(12, tr), {12, 'Aralik', "Aralik"}, month_sname)++
	assertEqual(month_name(13, tr), {13, 'not_found', "not_found"}, month_sname)++
	assertEqual(month_sname(DateTime), {2, 'Feb', "Feb"}, 'month_sname by datetime')++
	assertEqual(month_sname(1), {1, 'Jan', "Jan"}, month_sname)++
	assertEqual(month_sname(2), {2, 'Feb', "Feb"}, month_sname)++
	assertEqual(month_sname(3), {3, 'Mar', "Mar"}, month_sname)++
	assertEqual(month_sname(4), {4, 'Apr', "Apr"}, month_sname)++
	assertEqual(month_sname(5), {5, 'May', "May"}, month_sname)++
	assertEqual(month_sname(6), {6, 'Jun', "Jun"}, month_sname)++
	assertEqual(month_sname(7), {7, 'Jul', "Jul"}, month_sname)++
	assertEqual(month_sname(8), {8, 'Aug', "Aug"}, month_sname)++
	assertEqual(month_sname(9), {9, 'Sep', "Sep"}, month_sname)++
	assertEqual(month_sname(10), {10, 'Oct', "Oct"}, month_sname)++
	assertEqual(month_sname(11), {11, 'Nov', "Nov"}, month_sname)++
	assertEqual(month_sname(12), {12, 'Dec', "Dec"}, month_sname)++
	assertEqual(month_sname(13), {13, 'not_found', "not_found"}, month_sname)++
	assertEqual(month_sname(DateTime, en), {2, 'Feb', "Feb"}, 'month_sname by datetime')++
	assertEqual(month_sname(1, en), {1, 'Jan', "Jan"}, month_sname)++
	assertEqual(month_sname(2, en), {2, 'Feb', "Feb"}, month_sname)++
	assertEqual(month_sname(3, en), {3, 'Mar', "Mar"}, month_sname)++
	assertEqual(month_sname(4, en), {4, 'Apr', "Apr"}, month_sname)++
	assertEqual(month_sname(5, en), {5, 'May', "May"}, month_sname)++
	assertEqual(month_sname(6, en), {6, 'Jun', "Jun"}, month_sname)++
	assertEqual(month_sname(7, en), {7, 'Jul', "Jul"}, month_sname)++
	assertEqual(month_sname(8, en), {8, 'Aug', "Aug"}, month_sname)++
	assertEqual(month_sname(9, en), {9, 'Sep', "Sep"}, month_sname)++
	assertEqual(month_sname(10, en), {10, 'Oct', "Oct"}, month_sname)++
	assertEqual(month_sname(11, en), {11, 'Nov', "Nov"}, month_sname)++
	assertEqual(month_sname(12, en), {12, 'Dec', "Dec"}, month_sname)++
	assertEqual(month_sname(13, en), {13, 'not_found', "not_found"}, month_sname)++
	assertEqual(month_sname(DateTime, tr), {2, 'Sub', "Sub"}, 'month_sname by datetime')++
	assertEqual(month_sname(1, tr), {1, 'Oca', "Oca"}, month_sname)++
	assertEqual(month_sname(2, tr), {2, 'Sub', "Sub"}, month_sname)++
	assertEqual(month_sname(3, tr), {3, 'Mar', "Mar"}, month_sname)++
	assertEqual(month_sname(4, tr), {4, 'Nis', "Nis"}, month_sname)++
	assertEqual(month_sname(5, tr), {5, 'May', "May"}, month_sname)++
	assertEqual(month_sname(6, tr), {6, 'Haz', "Haz"}, month_sname)++
	assertEqual(month_sname(7, tr), {7, 'Tem', "Tem"}, month_sname)++
	assertEqual(month_sname(8, tr), {8, 'Agu', "Agu"}, month_sname)++
	assertEqual(month_sname(9, tr), {9, 'Eyl', "Eyl"}, month_sname)++
	assertEqual(month_sname(10, tr), {10, 'Eki', "Eki"}, month_sname)++
	assertEqual(month_sname(11, tr), {11, 'Kas', "Kas"}, month_sname)++
	assertEqual(month_sname(12, tr), {12, 'Ara', "Ara"}, month_sname)++
	assertEqual(month_sname(13, tr), {13, 'not_found', "not_found"}, month_sname)
,

	if All =:= true ->	
		Results = [{if T -> pass; true -> 'FAIL' end, G} || {T, G} <- TestResults];
	 true ->
		Results = [{if T -> pass; true -> 'FAIL' end, G} || {T, G} <- TestResults, T =:= false]
	end,

	if 
		All =:= true ->
		   io:format("~p~n", [Results]),
		   "All tests completed. Use calex_TEST:test(all) for complete result";
		length(Results) /= 0 ->
		   io:format("~p~n", [Results]);
		true ->
		   "All tests passed. Use calex_TEST:test(all) for complete result"
	end.

assertEqual(LeftPredicate, RightPredicate, FunctionName) ->
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
