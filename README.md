#Module calex 

[Overview](overview-summary.html)

[![erlang logo](erlang.png)](http://www.erlang.org/)

* * *

Module calex
============

*   [Description](#description)
*   [Function Index](#index)
*   [Function Details](#functions)

Ruby inspired calendar extension functions for Calendar module of Erlang.

Copyright © GPL 3.0 license

**Version:** 0.0.1

**Authors:** Aylin Ahmed ([aylin.ahmed@jetbot.co.uk](mailto:aylin.ahmed@jetbot.co.uk)).

Description
-----------

Ruby inspired calendar extension functions for Calendar module of Erlang. This is a beta version of calendar extensions. Please be cautious while using it as it's not tested in any production environment yet thanks for your interest in trying it...

Function Index
--------------

[dayname\_of\_the\_week/0](#dayname_of_the_week-0)

Provides information of the current day in the current week in English.

[dayname\_of\_the\_week/1](#dayname_of_the_week-1)

Provides information of the day for the given datetime or given day number in English.

[dayname\_of\_the\_week/2](#dayname_of_the_week-2)

Provides information of the current day for the given date time or given day number for the given ISO639-1 Language Code.

[days\_between/2](#days_between-2)

Provides total difference in days between given two datetime.

[days\_from\_now/1](#days_from_now-1)

Provides total difference in days between current datetime and given datetime.

[days\_since\_begin\_of\_week/0](#days_since_begin_of_week-0)

Provides total days spent since the begining of the week for current datetime.

[days\_since\_begin\_of\_week/1](#days_since_begin_of_week-1)

Provides total days spent since the begining of the week for the given datetime.

[days\_since\_begin\_of\_year/0](#days_since_begin_of_year-0)

Provides total days spent since the begining of the year for current datetime.

[days\_since\_begin\_of\_year/1](#days_since_begin_of_year-1)

Provides total days spent since the begining of the year for the given datetime.

[days\_until\_end\_of\_week/0](#days_until_end_of_week-0)

Provides total days between current datetime and the end of the current week.

[days\_until\_end\_of\_week/1](#days_until_end_of_week-1)

Provides total days between given datetime and the end of the given datetime's week.

[days\_until\_end\_of\_year/0](#days_until_end_of_year-0)

Provides total days between current datetime and the end of the current datetime's year.

[days\_until\_end\_of\_year/1](#days_until_end_of_year-1)

Provides total days between given datetime and the end of the given datetime's year.

[days\_until\_now/1](#days_until_now-1)

Provides total difference in days between given datetime and current day.

[end\_of\_the\_day/0](#end_of_the_day-0)

Provides End of the Day information as datetime for current datetime.

[end\_of\_the\_day/1](#end_of_the_day-1)

Provides End of the Day information as datetime for the given datetime.

[end\_of\_the\_hour/0](#end_of_the_hour-0)

Provides End of the Hour information as datetime for current datetime.

[end\_of\_the\_hour/1](#end_of_the_hour-1)

Provides End of the Hour information as datetime for the given datetime.

[end\_of\_the\_minute/0](#end_of_the_minute-0)

Provides End of the Minute information as datetime for the current datetime.

[end\_of\_the\_minute/1](#end_of_the_minute-1)

Provides End of the Minute information as datetime for the given datetime.

[end\_of\_the\_month/0](#end_of_the_month-0)

Provides End of the Month information as datetime for current datetime.

[end\_of\_the\_month/1](#end_of_the_month-1)

Provides End of the Month information as datetime for the given datetime.

[end\_of\_the\_week/0](#end_of_the_week-0)

Provides End of the Week information as datetime for current datetime.

[end\_of\_the\_week/1](#end_of_the_week-1)

Provides End of the Week information as datetime for the given datetime.

[epoch/0](#epoch-0)

Provides Unix epoch as integer.

[epoch\_start\_date/0](#epoch_start_date-0)

Provides Unix epoch start date information.

[format/2](#format-2)

Provides formatted reresentation of the given datetime by Ruby inspired date time formats.

[last\_month/0](#last_month-0)

Provides Last Month's information as datetime for current datetime.

[last\_month/1](#last_month-1)

Provides Last Month's information as datetime for the given datetime.

[last\_year/0](#last_year-0)

Provides Last Year's information as datetime for current datetime.

[last\_year/1](#last_year-1)

Provides Last Year's information as datetime for the given datetime.

[middle\_of\_the\_day/0](#middle_of_the_day-0)

Provides Mid of the Day information as datetime for the current datetime.

[middle\_of\_the\_day/1](#middle_of_the_day-1)

Provides Mid of the Day information as datetime for the given datetime.

[month\_name/0](#month_name-0)

Provides month name for current datetime.

[month\_name/1](#month_name-1)

Provides month name for the given datetime or month number.

[month\_name/2](#month_name-2)

Provides month name for the given datetime or month number for the given ISO639-1 Language code.

[next\_month/0](#next_month-0)

Provides Next Month's information as datetime for current datetime.

[next\_month/1](#next_month-1)

Provides Next Month's information as datetime for the given datetime.

[next\_year/0](#next_year-0)

Provides Next Year's information as datetime for current datetime.

[next\_year/1](#next_year-1)

Provides Next Year's information as datetime for the given datetime.

[seconds\_between/2](#seconds_between-2)

Provides total difference in seconds between given two datetime.

[seconds\_since\_begin\_of\_year/0](#seconds_since_begin_of_year-0)

Provides Total seconds spent since the beginning of the year information as datetime for current datetime.

[seconds\_since\_begin\_of\_year/1](#seconds_since_begin_of_year-1)

Provides Total seconds spent since the beginning of the year information as datetime for the given datetime.

[seconds\_since\_midnight/0](#seconds_since_midnight-0)

Provides Total seconds spent since the midnight information as datetime for current datetime.

[seconds\_since\_midnight/1](#seconds_since_midnight-1)

Provides Total seconds spent since the midnight information as datetime for the given datetime.

[seconds\_until\_end\_of\_day/0](#seconds_until_end_of_day-0)

Provides total seconds from current datetime and the end of current datetime's day.

[seconds\_until\_end\_of\_day/1](#seconds_until_end_of_day-1)

Provides total seconds from given datetime and the end of given datetime's day.

[seconds\_until\_end\_of\_year/0](#seconds_until_end_of_year-0)

Provides total seconds from currnet datetime and the end of the current year.

[seconds\_until\_end\_of\_year/1](#seconds_until_end_of_year-1)

Provides total seconds from given datetime and the end of given datetime's last day in its year.

[start\_of\_the\_day/0](#start_of_the_day-0)

Provides Start of the Day information as datetime for current datetime.

[start\_of\_the\_day/1](#start_of_the_day-1)

Provides Start of the Day information as datetime for the given datetime.

[start\_of\_the\_hour/0](#start_of_the_hour-0)

Provides Start of the Hour information as datetime for current datetime.

[start\_of\_the\_hour/1](#start_of_the_hour-1)

Provides Start of the Hour information as datetime for the given datetime.

[start\_of\_the\_minute/0](#start_of_the_minute-0)

Provides Start of the Minute information as datetime for current datetime.

[start\_of\_the\_minute/1](#start_of_the_minute-1)

Provides Start of the Minute information as datetime for the given datetime.

[start\_of\_the\_month/0](#start_of_the_month-0)

Provides Start of the Day information as datetime for current datetime.

[start\_of\_the\_month/1](#start_of_the_month-1)

Provides Start of the Day information as datetime for the given datetime.

[start\_of\_the\_week/0](#start_of_the_week-0)

Provides Start of the Week information as datetime for current datetime.

[start\_of\_the\_week/1](#start_of_the_week-1)

Provides Start of the Week information as datetime for the given datetime.

[tomorrow/0](#tomorrow-0)

Provides Tomorrow's information as datetime for current day.

[tomorrow/1](#tomorrow-1)

Provides Tomorrow's information as datetime for the given datetime.

[total\_seconds/1](#total_seconds-1)

Provides total seconds by day, hour or minute.

[until\_epoch/0](#until_epoch-0)

Provides time spent since the AD.

[week\_number/0](#week_number-0)

Provides week number of current datetime.

[week\_number/1](#week_number-1)

Provides week number of the given datetime.

[yesterday/0](#yesterday-0)

Provides Yesterday's information as datetime for current day.

[yesterday/1](#yesterday-1)

Provides Yesterday's information as datetime for the given datetime.

Function Details
----------------

### dayname\_of\_the\_week/0

dayname\_of\_the\_week() -> {integer(), string(), string()}  

returns: Day number starting from 1, long name of the day and short name of the day

Provides information of the current day in the current week in English.

### dayname\_of\_the\_week/1

dayname\_of\_the\_week(DayNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> {integer(), string(), string()}  

returns: Day number starting from 1, long name of the day and short name of the day

Provides information of the day for the given datetime or given day number in English.

### dayname\_of\_the\_week/2

dayname\_of\_the\_week(DayNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), LangCode::atom()) -> {integer(), string(), string()}  

returns: Day number starting from 1, long name of the day and short name of the day

Provides information of the current day for the given date time or given day number for the given ISO639-1 Language Code

### days\_between/2

days\_between(DateTimeFrom::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), DateTimeTo::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total days between two days as integer.

Provides total difference in days between given two datetime. Second datetime's day is NOT counted within total days.

### days\_from\_now/1

days\_from\_now(DateTimeTo::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total days between current datetime and given datetime as integer.

Provides total difference in days between current datetime and given datetime. Given datetime's day is NOT counted within total days.

### days\_since\_begin\_of\_week/0

days\_since\_begin\_of\_week() -> integer()  

returns: Total days spent since the beginning of the week as integer().

Provides total days spent since the begining of the week for current datetime. Current day is also counted within total days.

### days\_since\_begin\_of\_week/1

days\_since\_begin\_of\_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total days spent since the beginning of the week as integer().

Provides total days spent since the begining of the week for the given datetime. Given datetime's day is also counted within total days.

### days\_since\_begin\_of\_year/0

days\_since\_begin\_of\_year() -> integer()  

returns: Total days spent since the beginning of the year as integer().

Provides total days spent since the begining of the year for current datetime. Current datetime is NOT counted within total days.

### days\_since\_begin\_of\_year/1

days\_since\_begin\_of\_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total days spent since the beginning of the year as integer().

Provides total days spent since the begining of the year for the given datetime. Current datetime is NOT counted within total days.

### days\_until\_end\_of\_week/0

days\_until\_end\_of\_week() -> integer()  

returns: Total days between current datetime and the end of current week as integer.

Provides total days between current datetime and the end of the current week.

### days\_until\_end\_of\_week/1

days\_until\_end\_of\_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total days between given datetime and the end of the given datetime's week as integer.

Provides total days between given datetime and the end of the given datetime's week.

### days\_until\_end\_of\_year/0

days\_until\_end\_of\_year() -> integer()  

returns: Total days between current datetime and the end of the current datetime's year as integer.

Provides total days between current datetime and the end of the current datetime's year.

### days\_until\_end\_of\_year/1

days\_until\_end\_of\_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total days between given datetime and the end of the given datetime's year as integer.

Provides total days between given datetime and the end of the given datetime's year.

### days\_until\_now/1

days\_until\_now(DateTimeFrom::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total days between given datetime and current datetime as integer.

Provides total difference in days between given datetime and current day. Current datetime's day is NOT counted within total days.

### end\_of\_the\_day/0

end\_of\_the\_day() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Day including last second of the day as datetime.

Provides End of the Day information as datetime for current datetime.

### end\_of\_the\_day/1

end\_of\_the\_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Day including last second of the day as datetime.

Provides End of the Day information as datetime for the given datetime.

### end\_of\_the\_hour/0

end\_of\_the\_hour() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Hour including last second of the day as datetime.

Provides End of the Hour information as datetime for current datetime.

### end\_of\_the\_hour/1

end\_of\_the\_hour(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Hour including last second of the day as datetime.

Provides End of the Hour information as datetime for the given datetime.

### end\_of\_the\_minute/0

end\_of\_the\_minute() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Minute including last second of the day as datetime.

Provides End of the Minute information as datetime for the current datetime.

### end\_of\_the\_minute/1

end\_of\_the\_minute(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Minute including last second of the day as datetime.

Provides End of the Minute information as datetime for the given datetime.

### end\_of\_the\_month/0

end\_of\_the\_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Month including last second of the day as datetime.

Provides End of the Month information as datetime for current datetime.

### end\_of\_the\_month/1

end\_of\_the\_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Month including last second of the day as datetime.

Provides End of the Month information as datetime for the given datetime.

### end\_of\_the\_week/0

end\_of\_the\_week() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Week including last second of the day as datetime.

Provides End of the Week information as datetime for current datetime.

### end\_of\_the\_week/1

end\_of\_the\_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: End of the Week including last second of the day as datetime.

Provides End of the Week information as datetime for the given datetime.

### epoch/0

epoch() -> integer()  

returns: Total seconds as integer.

Provides Unix epoch as integer.

### epoch\_start\_date/0

epoch\_start\_date() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Epoch start date in datetime.

Provides Unix epoch start date information.

### format/2

format(Format::string(), DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> string()  

returns: Formatted datetime string as state in string formatters.

Provides formatted reresentation of the given datetime by Ruby inspired date time formats.

 	--- CONVERSION SPECIFIERS ---
  %Y - Year including century, zero-padded: 2022
  %y - Year without century, in range (0.99), zero-padded: 22
  %C - Century, zero-padded: 20
  %m - Month of the year, in range (1..12), zero-padded: 01
  %B - Full month name, capitalized: January
  %b - Abbreviated month name, capitalized: Jan
  %h - Same as %b.
  %d - Day of the month, in range (1..31), zero-padded: 01
  %e - Day of the month, in range (1..31), blank-padded: " 1"
  %j - Day of the year, in range (1..366), zero-padded: 125
  %H - Hour of the day, in range (0..23), zero-padded: 12
  %k - Hour of the day, in range (0..23), blank-padded: " 1"
  %I - Hour of the day, in range (1..12), zero-padded: 08
  %l - Hour of the day, in range (1..12), blank-padded: " 8"
  %P - Meridian indicator, lowercase: am, pm
  %p - Meridian indicator, uppercase: AM, PM
  %M - Minute of the hour, in range (0..59), zero-padded: 57
  %S - Second of the minute in range (0..59), zero-padded: 18
  %s - Number of seconds since the epoch: 1656505136
  %A - Full weekday name: Wednesday
  %a - Abbreviated weekday name: Wed
  %u - Day of the week, in range (1..7), Monday is 1: 2
  %w - Day of the week, in range (0..6), Sunday is 0: 0
  %W - Week number of the year, in range (0..53), zero-padded, where each week begins on a Monday: 26
 
  	--- SHORTHAND CONVERSION SPECIFIERS ---
  %c - Date and time: "Wed Jun 29 08:01:41 2022", shorthand for '%a %b %e %T %Y'
  %D - Date: "06/29/22", shorthand for '%m/%d/%y'
  %F - ISO 8601 date: "2022-06-29", shorthand for '%Y-%m-%d'
  %v - VMS date: "29-JUN-2022", shorthand for '%e-%^b-%Y'
  %x - Same as %D
  %X - Same as %T
  %r - 12-hour time:  "01:00:00 AM", shorthand for '%I:%M:%S %p'
  %R - 24-hour time: "01:00", shorthand for '%H:%M'
  %T - 24-hour time: "01:00:00", shorthand for '%H:%M:%S'
  %+ - Date and time: "Wed Jun 29 08:31:53 -05:00 2022", shorthand for '%a %b %e %H:%M:%S %Z %Y'

### last\_month/0

last\_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Last Month as datetime.

Provides Last Month's information as datetime for current datetime.

### last\_month/1

last\_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Last Month as datetime.

Provides Last Month's information as datetime for the given datetime.

### last\_year/0

last\_year() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Last Year as datetime.

Provides Last Year's information as datetime for current datetime.

### last\_year/1

last\_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Last Year as datetime.

Provides Last Year's information as datetime for the given datetime.

### middle\_of\_the\_day/0

middle\_of\_the\_day() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Mid of the Day as datetime.

Provides Mid of the Day information as datetime for the current datetime.

### middle\_of\_the\_day/1

middle\_of\_the\_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Mid of the Day as datetime.

Provides Mid of the Day information as datetime for the given datetime.

### month\_name/0

month\_name() -> {integer(), string(), string()}  

returns: Month numbers starting from 1, long month name, short month name.

Provides month name for current datetime.

### month\_name/1

month\_name(MonthNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> {integer(), string(), string()}  

returns: Month numbers starting from 1, long month name, short month name.

Provides month name for the given datetime or month number.

### month\_name/2

month\_name(MonthNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), LangCode::atom()) -> {integer(), string(), string()}  

returns: Month numbers starting from 1, long month name, short month name.

Provides month name for the given datetime or month number for the given ISO639-1 Language code.

### next\_month/0

next\_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Next Month as datetime.

Provides Next Month's information as datetime for current datetime.

### next\_month/1

next\_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Next Month as datetime.

Provides Next Month's information as datetime for the given datetime.

### next\_year/0

next\_year() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Next Year as datetime.

Provides Next Year's information as datetime for current datetime.

### next\_year/1

next\_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Next Year as datetime.

Provides Next Year's information as datetime for the given datetime.

### seconds\_between/2

seconds\_between(DateTimeFrom::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), DateTimeTo::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total seconds between two days as integer.

Provides total difference in seconds between given two datetime.

### seconds\_since\_begin\_of\_year/0

seconds\_since\_begin\_of\_year() -> integer()  

returns: Total seconds spent since the beginning of the year as integer.

Provides Total seconds spent since the beginning of the year information as datetime for current datetime.

### seconds\_since\_begin\_of\_year/1

seconds\_since\_begin\_of\_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total seconds spent since the beginning of the year as integer.

Provides Total seconds spent since the beginning of the year information as datetime for the given datetime.

### seconds\_since\_midnight/0

seconds\_since\_midnight() -> integer()  

returns: Total seconds spent since the midnight as integer.

Provides Total seconds spent since the midnight information as datetime for current datetime.

### seconds\_since\_midnight/1

seconds\_since\_midnight(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total seconds spent since the midnight as integer.

Provides Total seconds spent since the midnight information as datetime for the given datetime.

### seconds\_until\_end\_of\_day/0

seconds\_until\_end\_of\_day() -> integer()  

returns: Total seconds until the end of the current datetime as integer

Provides total seconds from current datetime and the end of current datetime's day.

### seconds\_until\_end\_of\_day/1

seconds\_until\_end\_of\_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total seconds until the end of the given datetime as integer

Provides total seconds from given datetime and the end of given datetime's day.

### seconds\_until\_end\_of\_year/0

seconds\_until\_end\_of\_year() -> integer()  

returns: Total seconds until the end of the current datetime's last day in the year as integer

Provides total seconds from currnet datetime and the end of the current year.

### seconds\_until\_end\_of\_year/1

seconds\_until\_end\_of\_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Total seconds until the end of the given datetime's last day as integer

Provides total seconds from given datetime and the end of given datetime's last day in its year.

### start\_of\_the\_day/0

start\_of\_the\_day() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Day as datetime.

Provides Start of the Day information as datetime for current datetime.

### start\_of\_the\_day/1

start\_of\_the\_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Day as datetime.

Provides Start of the Day information as datetime for the given datetime.

### start\_of\_the\_hour/0

start\_of\_the\_hour() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Hour as datetime.

Provides Start of the Hour information as datetime for current datetime.

### start\_of\_the\_hour/1

start\_of\_the\_hour(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Hour as datetime.

Provides Start of the Hour information as datetime for the given datetime.

### start\_of\_the\_minute/0

start\_of\_the\_minute() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Minute as datetime.

Provides Start of the Minute information as datetime for current datetime.

### start\_of\_the\_minute/1

start\_of\_the\_minute(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Minute as datetime.

Provides Start of the Minute information as datetime for the given datetime.

### start\_of\_the\_month/0

start\_of\_the\_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Day as datetime.

Provides Start of the Day information as datetime for current datetime.

### start\_of\_the\_month/1

start\_of\_the\_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Day as datetime.

Provides Start of the Day information as datetime for the given datetime.

### start\_of\_the\_week/0

start\_of\_the\_week() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Week as datetime.

Provides Start of the Week information as datetime for current datetime.

### start\_of\_the\_week/1

start\_of\_the\_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Start of the Week as datetime.

Provides Start of the Week information as datetime for the given datetime.

### tomorrow/0

tomorrow() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Tomorrow as datetime.

Provides Tomorrow's information as datetime for current day.

### tomorrow/1

tomorrow(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Tomorrow as datetime.

Provides Tomorrow's information as datetime for the given datetime.

### total\_seconds/1

total\_seconds(X1::atom()) -> integer()  

returns: Total seconds as integer.

Provides total seconds by day, hour or minute.

### until\_epoch/0

until\_epoch() -> integer()  

returns: Total seconds in between AD and Unix epoch.

Provides time spent since the AD.

### week\_number/0

week\_number() -> integer()  

returns: Week number of current datetime as integer.

Provides week number of current datetime.

### week\_number/1

week\_number(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> integer()  

returns: Week number of the given datetime as integer.

Provides week number of the given datetime.

### yesterday/0

yesterday() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Yesterday as datetime.

Provides Yesterday's information as datetime for current day.

### yesterday/1

yesterday(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  

returns: Yesterday as datetime.

Provides Yesterday's information as datetime for the given datetime.

* * *

[Overview](overview-summary.html)

[![erlang logo](erlang.png)](http://www.erlang.org/)

_Generated by EDoc_
