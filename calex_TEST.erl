-module(calex_TEST).
-export([test/0]).
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
         dayname_of_the_week/1
       ]
       ).


test() ->
        DateTime = {{2023, 02, 23}, {18, 31, 01}},
	test_individual(DateTime).

test_individual(DateTime) ->
	io:format("TEST Against Date Time ~p~n", [DateTime]),
	{Date, Time} = DateTime,
	{Hour, Minute, Second} = Time,
	eval(tomorrow(DateTime), {2023,02,24}, tomorrow),
        eval(yesterday(DateTime), {2023, 02, 22}, yesterday),
	eval(next_year(DateTime), {{2024, 02, 23}, Time}, next_year),
	eval(last_year(DateTime), {{2022,02,23},Time}, last_year),
	eval(end_of_the_week(DateTime), {{2023, 02, 26},{23,59,59}}, end_of_the_week),
	eval(end_of_the_day(DateTime), {Date, {23,59,59}}, end_of_the_day),
	eval(end_of_the_hour(DateTime), {Date, {Hour, 59, 59}},end_of_the_hour),
	eval(end_of_the_minute(DateTime), {Date, {Hour, Minute, 59}}, end_of_the_minute),
	eval(start_of_the_week(DateTime), {{2023, 02, 19}, {0,0,0}}, start_of_the_week),
	eval(start_of_the_day(DateTime), {Date, {0,0,0}}, start_of_the_day),
	eval(start_of_the_hour(DateTime), {Date, {Hour,0, 0}}, start_of_the_hour),
	eval(start_of_the_minute(DateTime), {Date, {Hour, Minute,0}}, start_of_the_minute),
	eval(seconds_since_midnight(DateTime), 66661, seconds_since_midnight),
	eval(seconds_until_end_of_day(DateTime), 19739, seconds_until_end_of_day).

eval(LeftPredicate, RightPredicate, FunctionName) ->
	eval(LeftPredicate =:= RightPredicate),
	io:format(": ~p(~n\t~p, ~n\t~p) ~n", [FunctionName, LeftPredicate, RightPredicate]).
eval(true) ->
	io:write(pass);
eval(false) ->
	io:write(fail).

