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

-spec total_seconds(atom()) -> integer().
total_seconds(day) -> 86400;
total_seconds(hour) -> trunc(total_seconds(day) / 24);
total_seconds(minute) -> trunc(total_seconds(hour) / 60).

-spec epoch() -> integer().
epoch() ->
	seconds_between(epoch_start_date(), calendar:local_time()).

-spec epoch_start_date() -> calendar:datetime().
epoch_start_date() -> {{1970, 1, 1}, {0, 0, 0}}.

-spec until_epoch() -> integer().
until_epoch() ->
	seconds_between({{1, 1, 1}, {0,0,0}}, calendar:local_time()).

-spec tomorrow(calendar:datetime()) -> calendar:datetime().
tomorrow(DateTime) ->
	{Tomorrow, _} = add(day, 1, DateTime),
	Tomorrow.
-spec tomorrow() -> calendar:datetime().
tomorrow() ->
	tomorrow(add(day, 1, calendar:local_time())).

-spec yesterday(calendar:datetime()) -> calendar:datetime(). 
yesterday(DateTime) ->
	{Yesterday, _} = add(day, -1, DateTime),
	Yesterday.

-spec yesterday() -> calendar:datetime().
yesterday() ->
	yesterday(calendar:local_time()).

-spec next_year(calendar:datetime()) -> calendar:datetime().
next_year(DateTime) ->
	add(year, 1, DateTime).

-spec next_year() -> calendar:datetime().
next_year() ->
	next_year(calendar:local_time()).

-spec last_year(calendar:datetime()) -> calendar:datetime().
last_year(DateTime) ->
	add(year, -1, DateTime).

-spec last_year() -> calendar:datetime().
last_year() ->
	last_year(calendar:local_time()).

-spec next_month(calendar:datetime()) -> calendar:datetime().
next_month(DateTime) ->
	add(month, 1, DateTime).

-spec next_month() -> calendar:datetime().
next_month() ->
	next_month(calendar:local_time()).

-spec last_month(calendar:datetime()) -> calendar:datetime().
last_month(DateTime) ->
	add(month, -1, DateTime).

-spec last_month() -> calendar:datetime().
last_month() ->
	last_month(calendar:local_time()).

-spec end_of_the_month(calendar:datetime()) -> calendar:datetime().
end_of_the_month(DateTime) ->
	{{NextMonthsYear, NextMonthsMonth, _}, _} = next_month(DateTime),
	LastDayOfThisMonth = add(day, -1, {{NextMonthsYear, NextMonthsMonth, 1}, {23, 59, 59} }),
	LastDayOfThisMonth.

-spec end_of_the_month() -> calendar:datetime().
end_of_the_month() ->
	end_of_the_month(calendar:local_time()).

-spec end_of_the_week(calendar:datetime()) -> calendar:datetime().
end_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DaysToEndOfTheWeek = 7-calendar:day_of_the_week(Date),
	add(day, DaysToEndOfTheWeek, {Date, {23,59,59}}).

-spec end_of_the_week() -> calendar:datetime().
end_of_the_week() ->
	end_of_the_week(calendar:local_time()).

-spec end_of_the_day(calendar:datetime()) -> calendar:datetime().
end_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {23, 59, 59}}.

-spec end_of_the_day() -> calendar:datetime().
end_of_the_day() ->
	end_of_the_day(calendar:local_time()).

-spec end_of_the_hour(calendar:datetime()) -> calendar:datetime().
end_of_the_hour(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, _, _} = Time,
	{Date, {Hour, 59, 59}}.

-spec end_of_the_hour() -> calendar:datetime().
end_of_the_hour() ->
	end_of_the_hour(calendar:local_time()).

-spec end_of_the_minute(calendar:datetime()) -> calendar:datetime().
end_of_the_minute(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, Minute, _} = Time,
	{Date, {Hour, Minute, 59}}.

-spec end_of_the_minute() -> calendar:datetime().
end_of_the_minute() ->
	end_of_the_minute(calendar:local_time()).

-spec middle_of_the_day(calendar:datetime()) -> calendar:datetime().
middle_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {12, 0, 0}}.

-spec middle_of_the_day() -> calendar:datetime().
middle_of_the_day() ->
	middle_of_the_day(calendar:local_time()).

-spec start_of_the_month(calendar:datetime()) -> calendar:datetime().
start_of_the_month(DateTime) ->
	{{Year, Month, _}, _} = DateTime,
	{{Year, Month, 1}, {0,0,0}}.

-spec start_of_the_month() -> calendar:datetime().
start_of_the_month() ->
	start_of_the_month(calendar:local_time()).

-spec start_of_the_week(calendar:datetime()) -> calendar:datetime().
start_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DaysToStartOfTheWeek = -day_of_the_week(Date),
	add(day, DaysToStartOfTheWeek, {Date, {0,0,0}}).

-spec start_of_the_week() -> calendar:datetime().
start_of_the_week() ->
	start_of_the_week(calendar:local_time()).

-spec start_of_the_day(calendar:datetime()) -> calendar:datetime().
start_of_the_day(DateTime) ->
	{Date, _} = DateTime,
	{Date, {0,0,0}}.

-spec start_of_the_day() -> calendar:datetime().
start_of_the_day() ->
	start_of_the_day(calendar:local_time()).

-spec start_of_the_hour(calendar:datetime()) -> calendar:datetime().
start_of_the_hour(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, _, _} = Time,
	{Date, {Hour, 0, 0}}.

-spec start_of_the_hour() -> calendar:datetime().
start_of_the_hour() ->
	start_of_the_hour(calendar:local_time()).

-spec start_of_the_minute(calendar:datetime()) -> calendar:datetime().
start_of_the_minute(DateTime) ->
	{Date, Time} = DateTime,
	{Hour, Minute, _} = Time,
	{Date, {Hour, Minute, 0}}.

-spec start_of_the_minute() -> calendar:datetime().
start_of_the_minute() ->
	start_of_the_minute(clendar:local_time()).

-spec  seconds_since_midnight(calendar:datetime()) -> integer().
seconds_since_midnight(DateTime) ->
	{Date, _} = DateTime,
	seconds_between({Date, {0,0,0}}, DateTime).

-spec  seconds_since_midnight() -> integer().
seconds_since_midnight() ->
	seconds_since_midnight(calendar:local_time()).


-spec seconds_since_begin_of_year(calendar:datetime()) -> integer().
seconds_since_begin_of_year(DateTime) ->
	{{Year, _, _}, _} = DateTime,
	seconds_between({{Year, 1, 1}, {0,0,0}}, DateTime). 

-spec seconds_since_begin_of_year() -> integer().
seconds_since_begin_of_year() ->
	seconds_since_begin_of_year(calendar:local_time()).

-spec days_since_begin_of_week() -> integer().
days_since_begin_of_week() ->
	not_implemented.

-spec days_since_begin_of_week(calendar:datetime()) -> integer().
days_since_begin_of_week(DateTime) ->
	{Number, _, _} = dayname_of_the_week(DateTime),
	Number.

-spec days_since_begin_of_year(calendar:datetime()) -> integer().
days_since_begin_of_year(DateTime) ->
	seconds_since_begin_of_year(DateTime) div total_seconds(day).

-spec days_since_begin_of_year() -> integer().
days_since_begin_of_year() ->
	days_since_begin_of_year(calendar:local_time()).

-spec seconds_between(calendar:datetime(), calendar:datetime()) -> integer().
seconds_between(DateTimeFrom, DateTimeTo) ->
	calendar:datetime_to_gregorian_seconds(DateTimeTo) - calendar:datetime_to_gregorian_seconds(DateTimeFrom).

-spec days_between(calendar:datetime(), calendar:datetime()) -> integer().
days_between(DateTimeFrom, DateTimeTo) ->
	seconds_between(DateTimeFrom, DateTimeTo) div total_seconds(day).

-spec days_until_now(calendar:datetime()) -> integer().
days_until_now(DateTimeFrom) ->
	days_between(DateTimeFrom, calendar:local_time()).

-spec days_from_now(calendar:datetime()) -> integer().
days_from_now(DateTimeTo) ->
	days_between(calendar:local_time(), DateTimeTo).

-spec  seconds_until_end_of_day(calendar:datetime()) -> integer().
seconds_until_end_of_day(DateTime) ->
	{Date, _} = DateTime,
	calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}) + 1 - calendar:datetime_to_gregorian_seconds(DateTime).

-spec  seconds_until_end_of_day() -> integer().
seconds_until_end_of_day() ->
	seconds_until_end_of_day(calendar:local_time()).

-spec  seconds_until_end_of_year(calendar:datetime()) -> integer().
seconds_until_end_of_year(DateTime) ->
	{{Year, _, _}, _} = DateTime,
	calendar:datetime_to_gregorian_seconds({{Year, 12, 31}, {23,59,59}}) + 1 - calendar:datetime_to_gregorian_seconds(DateTime).

-spec  seconds_until_end_of_year() -> integer().
seconds_until_end_of_year() ->
	seconds_until_end_of_year(calendar:local_time()).

-spec days_until_end_of_week() -> integer().
days_until_end_of_week() ->
	days_until_end_of_week(calendar:local_time()).

-spec days_until_end_of_week(calendar:datetime()) -> integer().
days_until_end_of_week(DateTime) ->
	{Number, _, _} = dayname_of_the_week(DateTime),
	7 -  Number.

-spec days_until_end_of_year(calendar:datetime()) -> integer().
days_until_end_of_year(DateTime) ->
	{{Year, _, _}, _} = DateTime,
	if Year rem 4 =:= 0 -> 366 - days_since_begin_of_year(DateTime);
	   true -> 365 - days_since_begin_of_year(DateTime)
	end.

-spec days_until_end_of_year() -> integer().
days_until_end_of_year() ->
	days_until_end_of_year(calendar:local_time()).

-spec week_number() -> integer().
week_number() ->
	week_number(calendar:local_time()).

-spec week_number(calendar:datetime()) -> integer().
week_number(DateTime) ->
	{Date, _} = DateTime,
	{_, WeekNumber} = calendar:iso_week_number(Date),
	WeekNumber.

add(year, Years, DateTime) ->
	{{Year, Month, Day}, Time} = DateTime,
	{{Year+Years, Month, Day}, Time};
add(month, Months, DateTime) ->
	{{Year, Month, Day}, Time} = DateTime,
	{AdditionalYearCountByGivenMonths, RemainderMonthCountByGivenMonths} = 	{Months div 12, Months rem 12},
	CandidateMonthValue = Month + RemainderMonthCountByGivenMonths,
	{ExtraYearAfterMonthAddition, MonthValue} = {CandidateMonthValue div 12, CandidateMonthValue rem 12},
	{{Year + AdditionalYearCountByGivenMonths + ExtraYearAfterMonthAddition, MonthValue, Day}, Time}; 
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

-spec month_name() -> {integer(), atom(), string()}.
month_name() ->
	month_name(calendar:local_time()).

-spec month_name(integer() | calendar:datetime()) -> {integer(), string(), string()}.
month_name(MonthNumber) when is_integer(MonthNumber) ->
	[{_, MonthNamesOfLang}] = [{L, M} || {L, M}<- ?MONTH_NAMES, L =:= en],
	month_name(MonthNumber, MonthNamesOfLang);
month_name(DateTime) ->
	{{_, MonthNumber, _}, _} = DateTime,
	month_name(MonthNumber).

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

-spec dayname_of_the_week() -> {integer(), _}.
dayname_of_the_week() ->
	dayname_of_the_week(calendar:local_time()).

-spec dayname_of_the_week(integer() | calendar:datetime()) -> {integer(), string(), string()}.
dayname_of_the_week(DayNumber) when is_integer(DayNumber) ->
	[{_, DayNames}] = [{L, D} || {L, D} <- ?DAY_NAMES, L =:= en],
        dayname_of_the_week(DayNumber, DayNames);
dayname_of_the_week(DateTime) ->
	{Date, _} = DateTime,
	DayNumber = calendar:day_of_the_week(Date),
        dayname_of_the_week(DayNumber).
	
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

%%	RUBY INSPIRED DATE TIME FORMAT STRINGS
%%
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
%%

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
	atom_to_list(till_implementing);
datepart('%+', DateTime) ->
	format("%a %b %e %H:%M:%S %Z %Y", DateTime);
datepart(AnyOther, _) ->
	atom_to_list(AnyOther) ++ " not_yet_implemented".
