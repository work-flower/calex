-module(calex).
-import(calendar, [
		   local_time/0, 
		   day_of_the_week/1, 
		   date_time_to_gregorian_seconds/1,
		   gregorian_seconds_to_date_time/1
		  ]).
-export([
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
	 dayname_of_the_week/0, 
	 dayname_of_the_week/1
	]).

tomorrow(DateTime) ->
	{Tomorrow, _} = add(day, 1, DateTime),
	Tomorrow.

tomorrow() ->
	tomorrow(add(day, 1, calendar:local_time())).


yesterday(DateTime) ->
	{Yesterday, _} = add(day, -1, DateTime),
	Yesterday.
yesterday() ->
	yesterday(calendar:local_time()).

next_year(DateTime) ->
	add(year, 1, DateTime).
next_year() ->
	next_year(calendar:local_time()).

last_year(DateTime) ->
	add(year, -1, DateTime).
last_year() ->
	last_year(calendar:local_time()).

end_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DaysToEndOfTheWeek = 7-calendar:day_of_the_week(Date),
	add(day, DaysToEndOfTheWeek, {Date, {23,59,59}}).
end_of_the_week() ->
	end_of_the_week(calendar:local_time()).

end_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {23, 59, 59}}.
end_of_the_day() ->
	end_of_the_day(calendar:local_time()).

end_of_the_hour(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, _, _} = Time,
	{Date, {Hour, 59, 59}}.
end_of_the_hour() ->
	end_of_the_hour(calendar:local_time()).

end_of_the_minute(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, Minute, _} = Time,
	{Date, {Hour, Minute, 59}}.
end_of_the_minute() ->
	end_of_the_minute(calendar:local_time()).

middle_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {12, 0, 0}}.
middle_of_the_day() ->
	middle_of_the_day(calendar:local_time()).

start_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DaysToStartOfTheWeek = -day_of_the_week(Date),
	add(day, DaysToStartOfTheWeek, {Date, {0,0,0}}).
start_of_the_week() ->
	start_of_the_week(calendar:local_time()).

start_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {0,0,0}}.
start_of_the_day() ->
	start_of_the_day(calendar:local_time()).

start_of_the_hour(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, _, _} = Time,
	{Date, {Hour, 0, 0}}.
start_of_the_hour() ->
	start_of_the_hour(calendar:local_time()).

start_of_the_minute(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, Minute, _} = Time,
	{Date, {Hour, Minute, 0}}.
start_of_the_minute() ->
	start_of_the_minute(clendar:local_time()).


add(year, Years, DateTime) ->
	{{Year, Month, Day}, Time} = DateTime,
	{{Year+Years, Month, Day}, Time};
add(month, Months, DateTime) ->
	not_yet_implemented;
add(day, Days, DateTime) ->
	DaysAsMilliseconds = Days * 24 * 60 * 60 * 1000,
	add(milliseconds, DaysAsMilliseconds, DateTime);
add(hour, Hours, DateTime) ->
	HoursAsMilliseconds = Hours * 60 * 60 * 1000,
	add(milliseconds, HoursAsMilliseconds, DateTime);
add(minute, Minutes, DateTime) ->
	MinutesAsMilliseconds = Minutes * 60 * 1000,
	add(milliseconds, MinutesAsMilliseconds, DateTime);
add(second, Seconds, DateTime) ->
	SecondsAsMilliseconds = Seconds * 1000,
	add(milliseconds, SecondsAsMilliseconds, DateTime);
add(milliseconds, Milliseconds, DateTime) ->
	TotalSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
	AddedTotalSeconds = TotalSeconds + trunc(Milliseconds/1000),
	calendar:gregorian_seconds_to_datetime(AddedTotalSeconds).

dayname_of_the_week() ->
	{Date, _} = calendar:local_time(),
	DayNumber = calendar:day_of_the_week(Date),
        dayname_of_the_week(DayNumber).

dayname_of_the_week(DayNumber) when is_integer(DayNumber) ->
        DayNames = [
                    {1, monday},
                    {2, tuesday},
                    {3, wednesday},
                    {4, thursday},
                    {5, friday},
                    {6, saturday},
                    {7, sunday}],
        dayname_of_the_week(DayNumber, DayNames).

dayname_of_the_week(DayNumber, [{DayNumber, DayName} | _]) ->
        {ok, {DayNumber, DayName}};
dayname_of_the_week(DayNumber, [_ | T]) ->
        dayname_of_the_week(DayNumber, T);
dayname_of_the_week(DayNumber, []) ->
	{error, {DayNumber, none}}.
