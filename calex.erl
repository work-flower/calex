% @author Aylin Ahmed <aylin.ahmed@jetbot.co.uk>
% @copyright GPL 3.0 license
% @version 0.0.1
% @doc Calendar extension functions for Calendar module of Erlang. This is a beta version of calendar extensions. Please be cautious while using it as it's not tested in any production environment yet thanks for your interest in trying it...

-module(calex).
-import(calendar, [
		   local_time/0, 
		   day_of_the_week/1, 
		   date_time_to_gregorian_seconds/1,
		   gregorian_seconds_to_date_time/1,
		   is_leap_year/1
		  ]).
-import(string, [slice/3, pad/4, uppercase/1, lowercase/1]).
-import(unicode, [characters_to_list/1]).
-include("calex.hrl").
-export([
	 total_seconds/1, epoch_start_date/0,	epoch/0,	until_epoch/0,
	 tomorrow/0,	tomorrow/1,
	 yesterday/0,	yesterday/1,
	 next_year/0,	next_year/1,
	 last_year/0,	last_year/1,
	 next_month/0,	next_month/1,
	 last_month/0,	last_month/1,
	 start_of_the_month/0,	 start_of_the_month/1,
	 start_of_the_week/0,	 start_of_the_week/1,
	 start_of_the_day/0,	 start_of_the_day/1,
	 start_of_the_hour/0,	 start_of_the_hour/1,
	 start_of_the_minute/0,	 start_of_the_minute/1,
	 middle_of_the_day/0,	 middle_of_the_day/1,
	 end_of_the_month/0,	 end_of_the_month/1,
	 end_of_the_week/0,	 end_of_the_week/1,
	 end_of_the_day/0,	 end_of_the_day/1,
	 end_of_the_hour/0,	 end_of_the_hour/1,
	 end_of_the_minute/0,	 end_of_the_minute/1,
	 seconds_since_midnight/0,	 seconds_since_midnight/1,
	 seconds_since_begin_of_year/0,	seconds_since_begin_of_year/1,
	 days_since_begin_of_week/0,	days_since_begin_of_week/1, 
	 days_since_begin_of_year/0,	days_since_begin_of_year/1,
	 days_between/2,	days_until_now/1,	days_from_now/1,
	 seconds_between/2,
	 week_number/0,		week_number/1,
	 seconds_until_end_of_day/0,	 seconds_until_end_of_day/1,
	 seconds_until_end_of_year/0,	 seconds_until_end_of_year/1,
	 days_until_end_of_week/0,	 days_until_end_of_week/1, 
	 days_until_end_of_year/0,	 days_until_end_of_year/1,
	 dayname_of_the_week/0, 	 dayname_of_the_week/1,		dayname_of_the_week/2,
	 month_name/0,	 month_name/1,	 month_name/2,
	 format/2
	]).

% @doc Provides total seconds by day, hour or minute.
% @returns Total seconds as integer.
-spec total_seconds(atom()) -> integer().
total_seconds(day) -> 86400;
total_seconds(hour) -> trunc(total_seconds(day) / 24);
total_seconds(minute) -> trunc(total_seconds(hour) / 60).

% @doc Provides Unix epoch as integer.
% @returns Total seconds as integer.
-spec epoch() -> integer().
epoch() ->
	seconds_between(epoch_start_date(), calendar:local_time()).
% @doc Provides Unix epoch start date information.
% @return Epoch start date in datetime.
-spec epoch_start_date() -> calendar:datetime().
epoch_start_date() -> {{1970, 1, 1}, {0, 0, 0}}.

% @doc Provides time spent since the AD.
% @returns Total seconds in between AD and Unix epoch.
-spec until_epoch() -> integer().
until_epoch() ->
	seconds_between({{1, 1, 1}, {0,0,0}}, calendar:local_time()).

% @doc Provides Tomorrow's information as datetime for the given datetime.
% @returns Tomorrow as datetime.
-spec tomorrow(calendar:datetime()) -> calendar:datetime().
tomorrow(DateTime) ->
	{Tomorrow, _} = add(day, 1, DateTime),
	Tomorrow.

% @doc Provides Tomorrow's information as datetime for current day.
% @returns Tomorrow as datetime.
-spec tomorrow() -> calendar:datetime().
tomorrow() ->
	tomorrow(add(day, 1, calendar:local_time())).

% @doc Provides Yesterday's information as datetime for the given datetime.
% @returns Yesterday as datetime.
-spec yesterday(calendar:datetime()) -> calendar:datetime(). 
yesterday(DateTime) ->
	{Yesterday, _} = add(day, -1, DateTime),
	Yesterday.

% @doc Provides Yesterday's information as datetime for current day.
% @returns Yesterday as datetime.
-spec yesterday() -> calendar:datetime().
yesterday() ->
	yesterday(calendar:local_time()).

% @doc Provides Next Year's information as datetime for the given datetime.
% @returns Next Year as datetime.
-spec next_year(calendar:datetime()) -> calendar:datetime().
next_year(DateTime) ->
	add(year, 1, DateTime).

% @doc Provides Next Year's information as datetime for current datetime.
% @returns Next Year as datetime.
-spec next_year() -> calendar:datetime().
next_year() ->
	next_year(calendar:local_time()).

% @doc Provides Last Year's information as datetime for the given datetime.
% @returns Last Year as datetime.
-spec last_year(calendar:datetime()) -> calendar:datetime().
last_year(DateTime) ->
	add(year, -1, DateTime).

% @doc Provides Last Year's information as datetime for current datetime.
% @returns Last Year as datetime.
-spec last_year() -> calendar:datetime().
last_year() ->
	last_year(calendar:local_time()).

% @doc Provides Next Month's information as datetime for the given datetime.
% @returns Next Month as datetime.
-spec next_month(calendar:datetime()) -> calendar:datetime().
next_month(DateTime) ->
	add(month, 1, DateTime).

% @doc Provides Next Month's information as datetime for current datetime.
% @returns Next Month as datetime.
-spec next_month() -> calendar:datetime().
next_month() ->
	next_month(calendar:local_time()).

% @doc Provides Last Month's information as datetime for the given datetime.
% @returns Last Month as datetime.
-spec last_month(calendar:datetime()) -> calendar:datetime().
last_month(DateTime) ->
	add(month, -1, DateTime).

% @doc Provides Last Month's information as datetime for current datetime.
% @returns Last Month as datetime.
-spec last_month() -> calendar:datetime().
last_month() ->
	last_month(calendar:local_time()).

% @doc Provides End of the Month information as datetime for the given datetime.
% @returns End of the Month including last second of the day as datetime.
-spec end_of_the_month(calendar:datetime()) -> calendar:datetime().
end_of_the_month(DateTime) ->
	{{NextMonthsYear, NextMonthsMonth, _}, _} = next_month(DateTime),
	LastDayOfThisMonth = add(day, -1, {{NextMonthsYear, NextMonthsMonth, 1}, {23, 59, 59} }),
	LastDayOfThisMonth.

% @doc Provides End of the Month information as datetime for current datetime.
% @returns End of the Month including last second of the day as datetime.
-spec end_of_the_month() -> calendar:datetime().
end_of_the_month() ->
	end_of_the_month(calendar:local_time()).

% @doc Provides End of the Week information as datetime for the given datetime.
% @returns End of the Week including last second of the day as datetime.
-spec end_of_the_week(calendar:datetime()) -> calendar:datetime().
end_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DaysToEndOfTheWeek = 7-calendar:day_of_the_week(Date),
	add(day, DaysToEndOfTheWeek, {Date, {23,59,59}}).

% @doc Provides End of the Week information as datetime for current datetime.
% @returns End of the Week including last second of the day as datetime.
-spec end_of_the_week() -> calendar:datetime().
end_of_the_week() ->
	end_of_the_week(calendar:local_time()).

% @doc Provides End of the Day information as datetime for the given datetime.
% @returns End of the Day including last second of the day as datetime.
-spec end_of_the_day(calendar:datetime()) -> calendar:datetime().
end_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {23, 59, 59}}.

% @doc Provides End of the Day information as datetime for current datetime.
% @returns End of the Day including last second of the day as datetime.
-spec end_of_the_day() -> calendar:datetime().
end_of_the_day() ->
	end_of_the_day(calendar:local_time()).

% @doc Provides End of the Hour information as datetime for the given datetime.
% @returns End of the Hour including last second of the day as datetime.
-spec end_of_the_hour(calendar:datetime()) -> calendar:datetime().
end_of_the_hour(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, _, _} = Time,
	{Date, {Hour, 59, 59}}.

% @doc Provides End of the Hour information as datetime for current datetime.
% @returns End of the Hour including last second of the day as datetime.
-spec end_of_the_hour() -> calendar:datetime().
end_of_the_hour() ->
	end_of_the_hour(calendar:local_time()).

% @doc Provides End of the Minute information as datetime for the given datetime.
% @returns End of the Minute including last second of the day as datetime.
-spec end_of_the_minute(calendar:datetime()) -> calendar:datetime().
end_of_the_minute(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, Minute, _} = Time,
	{Date, {Hour, Minute, 59}}.

% @doc Provides End of the Minute information as datetime for the current datetime.
% @returns End of the Minute including last second of the day as datetime.
-spec end_of_the_minute() -> calendar:datetime().
end_of_the_minute() ->
	end_of_the_minute(calendar:local_time()).

% @doc Provides Mid of the Day information as datetime for the given datetime.
% @returns Mid of the Day as datetime.
-spec middle_of_the_day(calendar:datetime()) -> calendar:datetime().
middle_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {12, 0, 0}}.

% @doc Provides Mid of the Day information as datetime for the current datetime.
% @returns Mid of the Day as datetime.
-spec middle_of_the_day() -> calendar:datetime().
middle_of_the_day() ->
	middle_of_the_day(calendar:local_time()).

% @doc Provides Start of the Day information as datetime for the given datetime.
% @returns Start of the Day as datetime.
-spec start_of_the_month(calendar:datetime()) -> calendar:datetime().
start_of_the_month(DateTime) ->
	{{Year, Month, _}, _} = DateTime,
	{{Year, Month, 1}, {0,0,0}}.

% @doc Provides Start of the Day information as datetime for current datetime.
% @returns Start of the Day as datetime.
-spec start_of_the_month() -> calendar:datetime().
start_of_the_month() ->
	start_of_the_month(calendar:local_time()).

% @doc Provides Start of the Week information as datetime for the given datetime.
% @returns Start of the Week as datetime.
-spec start_of_the_week(calendar:datetime()) -> calendar:datetime().
start_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DaysToStartOfTheWeek = -day_of_the_week(Date),
	add(day, DaysToStartOfTheWeek, {Date, {0,0,0}}).

% @doc Provides Start of the Week information as datetime for current datetime.
% @returns Start of the Week as datetime.
-spec start_of_the_week() -> calendar:datetime().
start_of_the_week() ->
	start_of_the_week(calendar:local_time()).

% @doc Provides Start of the Day information as datetime for the given datetime.
% @returns Start of the Day as datetime.
-spec start_of_the_day(calendar:datetime()) -> calendar:datetime().
start_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {0,0,0}}.

% @doc Provides Start of the Day information as datetime for current datetime.
% @returns Start of the Day as datetime.
-spec start_of_the_day() -> calendar:datetime().
start_of_the_day() ->
	start_of_the_day(calendar:local_time()).

% @doc Provides Start of the Hour information as datetime for the given datetime.
% @returns Start of the Hour as datetime.
-spec start_of_the_hour(calendar:datetime()) -> calendar:datetime().
start_of_the_hour(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, _, _} = Time,
	{Date, {Hour, 0, 0}}.

% @doc Provides Start of the Hour information as datetime for current datetime.
% @returns Start of the Hour as datetime.
-spec start_of_the_hour() -> calendar:datetime().
start_of_the_hour() ->
	start_of_the_hour(calendar:local_time()).

% @doc Provides Start of the Minute information as datetime for the given datetime.
% @returns Start of the Minute as datetime.
-spec start_of_the_minute(calendar:datetime()) -> calendar:datetime().
start_of_the_minute(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, Minute, _} = Time,
	{Date, {Hour, Minute, 0}}.

% @doc Provides Start of the Minute information as datetime for current datetime.
% @returns Start of the Minute as datetime.
-spec start_of_the_minute() -> calendar:datetime().
start_of_the_minute() ->
	start_of_the_minute(clendar:local_time()).

% @doc Provides Total seconds spent since the midnight information as datetime for the given datetime.
% @returns Total seconds spent since the midnight as integer.
-spec  seconds_since_midnight(calendar:datetime()) -> integer().
seconds_since_midnight(DateTime) ->
	{Date, _} = DateTime,
	seconds_between({Date, {0,0,0}}, DateTime).

% @doc Provides Total seconds spent since the midnight information as datetime for current datetime.
% @returns Total seconds spent since the midnight as integer.
-spec  seconds_since_midnight() -> integer().
seconds_since_midnight() ->
	seconds_since_midnight(calendar:local_time()).

% @doc Provides Total seconds spent since the beginning of the year information as datetime for the given datetime.
% @returns Total seconds spent since the beginning of the year as integer.
-spec seconds_since_begin_of_year(calendar:datetime()) -> integer().
seconds_since_begin_of_year(DateTime) ->
	{{Year, _, _}, _} = DateTime,
	seconds_between({{Year, 1, 1}, {0,0,0}}, DateTime). 

% @doc Provides Total seconds spent since the beginning of the year information as datetime for current datetime.
% @returns Total seconds spent since the beginning of the year as integer.
-spec seconds_since_begin_of_year() -> integer().
seconds_since_begin_of_year() ->
	seconds_since_begin_of_year(calendar:local_time()).
% @doc Provides total days spent since the begining of the week for current datetime. Current day is also counted within total days.
% @returns Total days spent since the beginning of the week as integer().
-spec days_since_begin_of_week() -> integer().
days_since_begin_of_week() ->
	days_since_begin_of_week(calendar:localtime()).

% @doc Provides total days spent since the begining of the week for the given datetime. Given datetime's day is also counted within total days.
% @returns Total days spent since the beginning of the week as integer().
-spec days_since_begin_of_week(calendar:datetime()) -> integer().
days_since_begin_of_week(DateTime) ->
	{Number, _, _} = dayname_of_the_week(DateTime),
	Number.

% @doc Provides total days spent since the begining of the year for the given datetime. Current datetime is NOT counted within total days.
% @returns Total days spent since the beginning of the year as integer().
-spec days_since_begin_of_year(calendar:datetime()) -> integer().
days_since_begin_of_year(DateTime) ->
	seconds_since_begin_of_year(DateTime) div total_seconds(day).

% @doc Provides total days spent since the begining of the year for current datetime. Current datetime is NOT counted within total days.
% @returns Total days spent since the beginning of the year as integer().
-spec days_since_begin_of_year() -> integer().
days_since_begin_of_year() ->
	days_since_begin_of_year(calendar:local_time()).

% @doc Provides total difference in seconds between given two datetime.
% @returns Total seconds between two days as integer.
-spec seconds_between(calendar:datetime(), calendar:datetime()) -> integer().
seconds_between(DateTimeFrom, DateTimeTo) ->
	calendar:datetime_to_gregorian_seconds(DateTimeTo) - calendar:datetime_to_gregorian_seconds(DateTimeFrom).

% @doc Provides total difference in days between given two datetime. Second datetime's day is NOT counted within total days.
% @returns Total days between two days as integer.
-spec days_between(calendar:datetime(), calendar:datetime()) -> integer().
days_between(DateTimeFrom, DateTimeTo) ->
	seconds_between(DateTimeFrom, DateTimeTo) div total_seconds(day).

% @doc Provides total difference in days between given datetime and current day. Current datetime's day is NOT counted within total days.
% @returns Total days between given datetime and current datetime as integer.
-spec days_until_now(calendar:datetime()) -> integer().
days_until_now(DateTimeFrom) ->
	days_between(DateTimeFrom, calendar:local_time()).

% @doc Provides total difference in days between current datetime and given datetime. Given datetime's day is NOT counted within total days.
% @returns Total days between current datetime and given datetime as integer.
-spec days_from_now(calendar:datetime()) -> integer().
days_from_now(DateTimeTo) ->
	days_between(calendar:local_time(), DateTimeTo).

% @doc Provides total seconds from given datetime and the end of given datetime's day.
% @returns Total seconds until the end of the given datetime as integer
-spec  seconds_until_end_of_day(calendar:datetime()) -> integer().
seconds_until_end_of_day(DateTime) ->
	{Date, _} = DateTime,
	calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}) + 1 - calendar:datetime_to_gregorian_seconds(DateTime).

% @doc Provides total seconds from current datetime and the end of current datetime's day.
% @returns Total seconds until the end of the current datetime as integer
-spec  seconds_until_end_of_day() -> integer().
seconds_until_end_of_day() ->
	seconds_until_end_of_day(calendar:local_time()).

% @doc Provides total seconds from given datetime and the end of given datetime's last day in its year.
% @returns Total seconds until the end of the given datetime's last day as integer
-spec  seconds_until_end_of_year(calendar:datetime()) -> integer().
seconds_until_end_of_year(DateTime) ->
	{{Year, _, _}, _} = DateTime,
	calendar:datetime_to_gregorian_seconds({{Year, 12, 31}, {23,59,59}}) + 1 - calendar:datetime_to_gregorian_seconds(DateTime).

% @doc Provides total seconds from currnet datetime and the end of the current year.
% @returns Total seconds until the end of the current datetime's last day in the year as integer
-spec  seconds_until_end_of_year() -> integer().
seconds_until_end_of_year() ->
	seconds_until_end_of_year(calendar:local_time()).

% @doc Provides total days between current datetime and the end of the current week.
% @returns Total days between current datetime and the end of current week as integer.
-spec days_until_end_of_week() -> integer().
days_until_end_of_week() ->
	days_until_end_of_week(calendar:local_time()).

% @doc Provides total days between given datetime and the end of the given datetime's week.
% @returns Total days between given datetime and the end of the given datetime's week as integer.
-spec days_until_end_of_week(calendar:datetime()) -> integer().
days_until_end_of_week(DateTime) ->
	{Number, _, _} = dayname_of_the_week(DateTime),
	7 -  Number.

% @doc Provides total days between given datetime and the end of the given datetime's year.
% @returns Total days between given datetime and the end of the given datetime's year as integer.
-spec days_until_end_of_year(calendar:datetime()) -> integer().
days_until_end_of_year(DateTime) ->
	{{Year, _, _}, _} = DateTime,
	if Year rem 4 =:= 0 -> 366 - days_since_begin_of_year(DateTime);
	   true -> 365 - days_since_begin_of_year(DateTime)
	end.

% @doc Provides total days between current datetime and the end of the current datetime's year.
% @returns Total days between current datetime and the end of the current datetime's year as integer.
-spec days_until_end_of_year() -> integer().
days_until_end_of_year() ->
	days_until_end_of_year(calendar:local_time()).

% @doc Provides week number of current datetime.
% @returns Week number of current datetime as integer.
-spec week_number() -> integer().
week_number() ->
	week_number(calendar:local_time()).

% @doc Provides week number of the given datetime.
% @returns Week number of the given datetime as integer.
-spec week_number(calendar:datetime()) -> integer().
week_number(DateTime) ->
	{Date, _} = DateTime,
	{_, WeekNumber} = calendar:iso_week_number(Date),
	WeekNumber.
% @private
add(year, Years, DateTime) ->
	{{Year, Month, Day}, Time} = DateTime,
	{{Year+Years, Month, Day}, Time};
% @private
add(month, Months, DateTime) ->
	{{Year, Month, Day}, Time} = DateTime,
	{AdditionalYearCountByGivenMonths, RemainderMonthCountByGivenMonths} = 	{Months div 12, Months rem 12},
	CandidateMonthValue = Month + RemainderMonthCountByGivenMonths,
	{ExtraYearAfterMonthAddition, MonthValue} = {CandidateMonthValue div 12, CandidateMonthValue rem 12},
	{{Year + AdditionalYearCountByGivenMonths + ExtraYearAfterMonthAddition, MonthValue, Day}, Time}; 
% @private
add(day, Days, DateTime) ->
	DaysAsMilliseconds = Days * 24 * 60 * 60 * 1000,
	add(milliseconds, DaysAsMilliseconds, DateTime);
% @private
add(hour, Hours, DateTime) ->
	HoursAsMilliseconds = Hours * 60 * 60 * 1000,
	add(milliseconds, HoursAsMilliseconds, DateTime);
% @private
add(minute, Minutes, DateTime) ->
	MinutesAsMilliseconds = Minutes * 60 * 1000,
	add(milliseconds, MinutesAsMilliseconds, DateTime);
% @private
add(second, Seconds, DateTime) ->
	SecondsAsMilliseconds = Seconds * 1000,
	add(milliseconds, SecondsAsMilliseconds, DateTime);
% @private
add(milliseconds, Milliseconds, DateTime) ->
	TotalSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
	AddedTotalSeconds = TotalSeconds + trunc(Milliseconds/1000),
	calendar:gregorian_seconds_to_datetime(AddedTotalSeconds).

% @doc Provides month name for current datetime.
% @returns Month numbers starting from 1, long month name, short month name.
-spec month_name() -> {integer(), string(), string()}.
month_name() ->
	month_name(calendar:local_time()).

% @doc Provides month name for the given datetime or month number.
% @returns Month numbers starting from 1, long month name, short month name.
-spec month_name(integer() | calendar:datetime()) -> {integer(), string(), string()}.
month_name(MonthNumber) when is_integer(MonthNumber) ->
	[{_, MonthNamesOfLang}] = [{L, M} || {L, M}<- ?MONTH_NAMES, L =:= en],
	month_name(MonthNumber, MonthNamesOfLang);
month_name(DateTime) ->
	{{_, MonthNumber, _}, _} = DateTime,
	month_name(MonthNumber).

% @doc Provides month name for the given datetime or month number for the given ISO639-1 Language code.
% @returns Month numbers starting from 1, long month name, short month name.
-spec month_name(integer() | calendar:datetime(), atom()) -> {integer(), string(), string()}.
month_name(MonthNumber, LangCode) when is_integer(MonthNumber), is_atom(LangCode) ->
	[{_, MonthNamesOfLang}] = [{L, M} || {L, M} <- ?MONTH_NAMES, L =:= LangCode],
	month_name(MonthNumber, MonthNamesOfLang);
month_name(DateTime, LangCode) when is_atom(LangCode) ->
	{{_, MonthNumber, _}, _} = DateTime,
	month_name(MonthNumber, LangCode);
month_name(MonthNumber, [{MonthNumber, LongMonthName, ShortMonthName} | _]) ->
	{MonthNumber, LongMonthName, ShortMonthName};
month_name(MonthNumber, [_ | T]) ->
	month_name(MonthNumber, T);
month_name(MonthNumber, []) ->
	{MonthNumber, "not_found", "not_found"}.

% @doc Provides information of the current day in the current week in English.
% @returns Day number starting from 1, long name of the day and short name of the day 
-spec dayname_of_the_week() -> {integer(), string(), string()}.
dayname_of_the_week() ->
	dayname_of_the_week(calendar:local_time()).

% @doc Provides information of the day for the given datetime or given day number in English.
% @returns Day number starting from 1, long name of the day and short name of the day 
-spec dayname_of_the_week(integer() | calendar:datetime()) -> {integer(), string(), string()}.
dayname_of_the_week(DayNumber) when is_integer(DayNumber) ->
	[{_, DayNames}] = [{L, D} || {L, D} <- ?DAY_NAMES, L =:= en],
        dayname_of_the_week(DayNumber, DayNames);
dayname_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DayNumber = calendar:day_of_the_week(Date),
        dayname_of_the_week(DayNumber).
	
% @doc Provides information of the current day for the given date time or given day number for the given ISO639-1 Language Code
% @returns Day number starting from 1, long name of the day and short name of the day 
-spec dayname_of_the_week(integer() | calendar:datetime(), atom()) -> {integer(), string(), string()}.
dayname_of_the_week(DayNumber, LangCode) when is_integer(DayNumber), is_atom(LangCode) ->
	[{_, DayNames}] = [{L, D} || {L, D} <- ?DAY_NAMES, L =:= LangCode],
        dayname_of_the_week(DayNumber, DayNames);
dayname_of_the_week(DateTime, LangCode) when is_atom(LangCode) ->
	{Date, _} = DateTime,
	DayNumber = calendar:day_of_the_week(Date),
	[{_, DayNames}] = [{L, D} || {L, D} <- ?DAY_NAMES, L =:= LangCode],
        dayname_of_the_week(DayNumber, DayNames);
dayname_of_the_week(DayNumber, [{DayNumber, LongDayName, ShortDayName} | _]) ->
        {DayNumber, LongDayName, ShortDayName};
dayname_of_the_week(DayNumber, [_ | T]) ->
        dayname_of_the_week(DayNumber, T);
dayname_of_the_week(DayNumber, []) ->
	{DayNumber, "not_found", "not_found"}.

% @doc	Provides formatted reresentation of the given datetime by Ruby inspired date time formats.
%%```
%%	--- CONVERSION SPECIFIERS ---
%% %Y - Year including century, zero-padded: 2022
%% %y - Year without century, in range (0.99), zero-padded: 22
%% %C - Century, zero-padded: 20 
%% %m - Month of the year, in range (1..12), zero-padded: 01
%% %B - Full month name, capitalized: January
%% %b - Abbreviated month name, capitalized: Jan
%% %h - Same as %b.
%% %d - Day of the month, in range (1..31), zero-padded: 01
%% %e - Day of the month, in range (1..31), blank-padded: " 1"
%% %j - Day of the year, in range (1..366), zero-padded: 125
%% %H - Hour of the day, in range (0..23), zero-padded: 12
%% %k - Hour of the day, in range (0..23), blank-padded: " 1"
%% %I - Hour of the day, in range (1..12), zero-padded: 08
%% %l - Hour of the day, in range (1..12), blank-padded: " 8"
%% %P - Meridian indicator, lowercase: am, pm
%% %p - Meridian indicator, uppercase: AM, PM
%% %M - Minute of the hour, in range (0..59), zero-padded: 57
%% %S - Second of the minute in range (0..59), zero-padded: 18
%% %s - Number of seconds since the epoch: 1656505136
%% %A - Full weekday name: Wednesday
%% %a - Abbreviated weekday name: Wed
%% %u - Day of the week, in range (1..7), Monday is 1: 2
%% %w - Day of the week, in range (0..6), Sunday is 0: 0
%% %W - Week number of the year, in range (0..53), zero-padded, where each week begins on a Monday: 26
%%
%% 	--- SHORTHAND CONVERSION SPECIFIERS ---
%% %c - Date and time: "Wed Jun 29 08:01:41 2022", shorthand for '%a %b %e %T %Y'
%% %D - Date: "06/29/22", shorthand for '%m/%d/%y'
%% %F - ISO 8601 date: "2022-06-29", shorthand for '%Y-%m-%d'
%% %v - VMS date: "29-JUN-2022", shorthand for '%e-%^b-%Y'
%% %x - Same as %D
%% %X - Same as %T
%% %r - 12-hour time:  "01:00:00 AM", shorthand for '%I:%M:%S %p'
%% %R - 24-hour time: "01:00", shorthand for '%H:%M'
%% %T - 24-hour time: "01:00:00", shorthand for '%H:%M:%S'
%% %+ - Date and time: "Wed Jun 29 08:31:53 -05:00 2022", shorthand for '%a %b %e %H:%M:%S %Z %Y'
%%
%% '''
% @returns Formatted datetime string as state in string formatters.
-spec format(string(), calendar:datetime()) -> string().
format(Format, DateTime) ->
	{match, Listed} = re:run(Format, "(\%[a-zA-Z\+])([0-9]*)", [global, {capture, all, list}]),
	RegexTokens = [NamedHead || [_, NamedHead, _] <- Listed], %ignore any arity for now
	SetRegexTokens = sets:from_list(RegexTokens),
	UniqueRegexTokens = sets:to_list(SetRegexTokens),
	format_internal(Format, UniqueRegexTokens, DateTime).

format_internal(Format, [H | T], DateTime) ->
	DatePartValue = datepart(list_to_atom(H), DateTime),
	Replaced = characters_to_list(re:replace(Format, H, DatePartValue)),
	format_internal(Replaced, T, DateTime);
format_internal(Format, [], _) ->
	Format.

datepart('%Y', DateTime) ->
	{{Year, _, _}, _} = DateTime,
	integer_to_list(Year);
datepart('%y', DateTime) ->
	{{Year, _, _}, _} = DateTime,
	slice(integer_to_list(Year), 2, 2);
datepart('%C', DateTime) ->
	{{Year, _, _}, _} = DateTime,
	pad(slice(integer_to_list(Year), 0, 2), 2, leading, " ");
datepart('%m', DateTime) ->
	{{_, Month, _}, _} = DateTime,
	pad(integer_to_list(Month), 2, leading, "0");
datepart('%B', DateTime) ->
	{{_, Month, _}, _} = DateTime,
	{_, Name, _} = month_name(Month),
	Name;
datepart('%b', DateTime) ->
	{{_, Month, _}, _} = DateTime,
	{_, _, ShortName} = month_name(Month),
	ShortName;
datepart('%h', DateTime) ->
	datepart('%b', DateTime);
datepart('%d', DateTime) ->
	{{_, _, Day}, _} = DateTime,
	pad(integer_to_list(Day), 2, leading, "0");
datepart('%e', DateTime) ->
	{{_, _, Day}, _} = DateTime,
	pad(integer_to_list(Day), 2, leading, " ");
datepart('%j', DateTime) ->
	integer_to_list(days_since_begin_of_year(DateTime));
datepart('%H', DateTime) ->
	{_, {Hour, _, _}} = DateTime,
	pad(integer_to_list(Hour), 2, leading, "0");
datepart('%k', DateTime) ->
	{_, {Hour, _, _}} = DateTime,
	pad(integer_to_list(Hour), 2, leading, " ");
datepart('%I', DateTime) ->
	{_, {Hour, _, _}} = DateTime,
	pad(integer_to_list(Hour rem 12), 2, leading, "0");
datepart('%l', DateTime) ->
	{_, {Hour, _, _}} = DateTime,
	pad(integer_to_list(Hour rem 12), 2, leading, " ");
datepart('%P', DateTime) ->
	{_, {Hour, _, _}} = DateTime,
	if
		Hour div 12 =:= 1 -> "PM";
		true -> "AM"
	end;
datepart('%p', DateTime) ->
	lowercase(datepart('%P', DateTime));
datepart('%M', DateTime) ->
	{_, {_, Minute, _}} = DateTime,
	pad(integer_to_list(Minute), 2, leading, "0");
datepart('%S', DateTime) ->
	{_, {_, _, Second}} = DateTime,
	pad(integer_to_list(Second), 2, leading, "0");
datepart('%s', DateTime) ->
	integer_to_list(seconds_between(epoch_start_date(),  DateTime));
datepart('%A', DateTime) ->
	{_, Name, _} = dayname_of_the_week(DateTime),
	Name;
datepart('%a', DateTime) ->
	{_, _, ShortName} = dayname_of_the_week(DateTime),
	ShortName;
datepart('%u', DateTime) ->
	{Number, _, _} = dayname_of_the_week(DateTime),
	integer_to_list(Number);
datepart('%w', DateTime) ->
	integer_to_list(list_to_integer(datepart('%u', DateTime)) -1);
datepart('%W', DateTime) ->
	integer_to_list(week_number(DateTime));
datepart('%c', DateTime) ->
	format("%a %b %e %T %Y", DateTime);
datepart('%D', DateTime) ->
	format("%m/%d/%Y", DateTime);
datepart('%F', DateTime) ->
	format("%Y-%m-%d", DateTime);
datepart('%v', DateTime) ->
	uppercase(format("%e-%b-%Y", DateTime));
datepart('%x', DateTime) ->
	datepart('%D', DateTime);
datepart('%X', DateTime) ->
	datepart('%T', DateTime);
datepart('%r', DateTime) ->
	format("%I:%M:%S %P", DateTime);
datepart('%R', DateTime) ->
	format("%H:%M", DateTime);
datepart('%T', DateTime) ->
	format("%H:%M:%S", DateTime);
datepart('%Z', DateTime) ->
	[UtcDateTime] = calendar:local_time_to_universal_time_dst(DateTime),
	SecondsDiff = seconds_between(UtcDateTime, DateTime),
	DateTime,
	atom_to_list(still_implementing);
datepart('%+', DateTime) ->
	format("%a %b %e %H:%M:%S %Z %Y", DateTime);
datepart(AnyOther, _) ->
	atom_to_list(AnyOther) ++ " not_yet_implemented".
