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
         end_of_the_day/0,
         end_of_the_day/1,
         start_of_the_day/0,
         start_of_the_day/1,
         start_of_the_week/0,
         start_of_the_week/1,
         end_of_the_week/0,
         end_of_the_week/1,
         dayname_of_the_week/0,
         dayname_of_the_week/1
       ]
       ).


test() ->
        DateTime = {{2023, 02, 23}, {18, 31, 01}},
	test_individual(DateTime).

test_individual(DateTime) ->
	{Date, Time} = DateTime,
	eval(tomorrow(DateTime), {2023,02,24}, tomorrow),
        eval(yesterday(DateTime), {2023, 02, 22}, yesterday),
	eval(next_year(DateTime), {{2024, 02, 23}, Time}, next_year),
	eval(last_year(DateTime), {{2022,02,23},Time}, last_year),
	eval(end_of_the_day(DateTime), {Date, {23,59,59}}, end_of_the_day),
	eval(start_of_the_day(DateTime), {Date, {0,0,0}}, start_of_the_day),
	eval(end_of_the_week(DateTime), {{2023, 02, 26},{23,59,59}}, end_of_the_week),
	eval(start_of_the_week(DateTime), {{2023, 02, 20}, {0,0,0}}, start_of_the_week).

eval(LeftPredicate, RightPredicate, FunctionName) ->
	eval(LeftPredicate =:= RightPredicate, string:join([FunctionName, ":", io:format("~p", [LeftPredicate]), "<->", io:format("~p", [RightPredicate])], " ")).
eval(true, Description) ->
	io:format("PASS: ~p~n", [Description]);
eval(false, Description) ->
	io:format("!>> FAIL: ~p~n", [Description]).

