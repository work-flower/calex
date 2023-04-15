Module calex
============

* [Description](#description)
* [Function Index](#function-index)
* [Function Details](#function-details)

Ruby inspired calendar extension functions for Calendar module of Erlang.

Copyright © GPL 3.0 license

**Version:** 0.0.1

**Authors:** Aylin Ahmed ([aylin.ahmed@jetbot.co.uk](mailto:aylin.ahmed@jetbot.co.uk)).

Description
-----------

Ruby inspired calendar extension functions for Calendar module of Erlang. This is a beta version of calendar extensions. Please be cautious while using it as it's not tested in any production environment yet thanks for your interest in trying it...

Function Index
--------------

| **Function** | **Description** |
| --- | --- |
| [dayname\_of\_the_week/0](#dayname_of_the_week0) | Provides information of the current day in the current week in English. |
| [dayname\_of\_the_week/1](#dayname_of_the_week1) | Provides information of the day for the given datetime or given day number in English. |
| [dayname\_of\_the_week/2](#dayname_of_the_week2) | Provides information of the current day for the given date time or given day number for the given ISO639-1 Language Code. |
| [days_between/2](#days_between2) | Provides total difference in days between given two datetime. |
| [days\_from\_now/1](#days_from_now1) | Provides total difference in days between current datetime and given datetime. |
| [days\_since\_begin\_of\_week/0](#days_since_begin_of_week0) | Provides total days spent since the begining of the week for current datetime. |
| [days\_since\_begin\_of\_week/1](#days_since_begin_of_week1) | Provides total days spent since the begining of the week for the given datetime. |
| [days\_since\_begin\_of\_year/0](#days_since_begin_of_year0) | Provides total days spent since the begining of the year for current datetime. |
| [days\_since\_begin\_of\_year/1](#days_since_begin_of_year1) | Provides total days spent since the begining of the year for the given datetime. |
| [days\_until\_end\_of\_week/0](#days_until_end_of_week0) | Provides total days between current datetime and the end of the current week. |
| [days\_until\_end\_of\_week/1](#days_until_end_of_week1) | Provides total days between given datetime and the end of the given datetime's week. |
| [days\_until\_end\_of\_year/0](#days_until_end_of_year0) | Provides total days between current datetime and the end of the current datetime's year. |
| [days\_until\_end\_of\_year/1](#days_until_end_of_year1) | Provides total days between given datetime and the end of the given datetime's year. |
| [days\_until\_now/1](#days_until_now1) | Provides total difference in days between given datetime and current day. |
| [end\_of\_the_day/0](#end_of_the_day0) | Provides End of the Day information as datetime for current datetime. |
| [end\_of\_the_day/1](#end_of_the_day1) | Provides End of the Day information as datetime for the given datetime. |
| [end\_of\_the_hour/0](#end_of_the_hour0) | Provides End of the Hour information as datetime for current datetime. |
| [end\_of\_the_hour/1](#end_of_the_hour1) | Provides End of the Hour information as datetime for the given datetime. |
| [end\_of\_the_minute/0](#end_of_the_minute0) | Provides End of the Minute information as datetime for the current datetime. |
| [end\_of\_the_minute/1](#end_of_the_minute1) | Provides End of the Minute information as datetime for the given datetime. |
| [end\_of\_the_month/0](#end_of_the_month0) | Provides End of the Month information as datetime for current datetime. |
| [end\_of\_the_month/1](#end_of_the_month1) | Provides End of the Month information as datetime for the given datetime. |
| [end\_of\_the_week/0](#end_of_the_week0) | Provides End of the Week information as datetime for current datetime. |
| [end\_of\_the_week/1](#end_of_the_week1) | Provides End of the Week information as datetime for the given datetime. |
| [epoch/0](#epoch0) | Provides Unix epoch as integer. |
| [epoch\_start\_date/0](#epoch_start_date0) | Provides Unix epoch start date information. |
| [format/2](#format2) | Provides formatted reresentation of the given datetime by Ruby inspired date time formats. |
| [last_month/0](#last_month0) | Provides Last Month's information as datetime for current datetime. |
| [last_month/1](#last_month1) | Provides Last Month's information as datetime for the given datetime. |
| [last_year/0](#last_year0) | Provides Last Year's information as datetime for current datetime. |
| [last_year/1](#last_year1) | Provides Last Year's information as datetime for the given datetime. |
| [middle\_of\_the_day/0](#middle_of_the_day0) | Provides Mid of the Day information as datetime for the current datetime. |
| [middle\_of\_the_day/1](#middle_of_the_day1) | Provides Mid of the Day information as datetime for the given datetime. |
| [month_name/0](#month_name0) | Provides month name for current datetime. |
| [month_name/1](#month_name1) | Provides month name for the given datetime or month number. |
| [month_name/2](#month_name2) | Provides month name for the given datetime or month number for the given ISO639-1 Language code. |
| [next_month/0](#next_month0) | Provides Next Month's information as datetime for current datetime. |
| [next_month/1](#next_month1) | Provides Next Month's information as datetime for the given datetime. |
| [next_year/0](#next_year0) | Provides Next Year's information as datetime for current datetime. |
| [next_year/1](#next_year1) | Provides Next Year's information as datetime for the given datetime. |
| [seconds_between/2](#seconds_between2) | Provides total difference in seconds between given two datetime. |
| [seconds\_since\_begin\_of\_year/0](#seconds_since_begin_of_year0) | Provides Total seconds spent since the beginning of the year information as datetime for current datetime. |
| [seconds\_since\_begin\_of\_year/1](#seconds_since_begin_of_year1) | Provides Total seconds spent since the beginning of the year information as datetime for the given datetime. |
| [seconds\_since\_midnight/0](#seconds_since_midnight0) | Provides Total seconds spent since the midnight information as datetime for current datetime. |
| [seconds\_since\_midnight/1](#seconds_since_midnight1) | Provides Total seconds spent since the midnight information as datetime for the given datetime. |
| [seconds\_until\_end\_of\_day/0](#seconds_until_end_of_day0) | Provides total seconds from current datetime and the end of current datetime's day. |
| [seconds\_until\_end\_of\_day/1](#seconds_until_end_of_day1) | Provides total seconds from given datetime and the end of given datetime's day. |
| [seconds\_until\_end\_of\_year/0](#seconds_until_end_of_year0) | Provides total seconds from currnet datetime and the end of the current year. |
| [seconds\_until\_end\_of\_year/1](#seconds_until_end_of_year1) | Provides total seconds from given datetime and the end of given datetime's last day in its year. |
| [start\_of\_the_day/0](#start_of_the_day0) | Provides Start of the Day information as datetime for current datetime. |
| [start\_of\_the_day/1](#start_of_the_day1) | Provides Start of the Day information as datetime for the given datetime. |
| [start\_of\_the_hour/0](#start_of_the_hour0) | Provides Start of the Hour information as datetime for current datetime. |
| [start\_of\_the_hour/1](#start_of_the_hour1) | Provides Start of the Hour information as datetime for the given datetime. |
| [start\_of\_the_minute/0](#start_of_the_minute0) | Provides Start of the Minute information as datetime for current datetime. |
| [start\_of\_the_minute/1](#start_of_the_minute1) | Provides Start of the Minute information as datetime for the given datetime. |
| [start\_of\_the_month/0](#start_of_the_month0) | Provides Start of the Day information as datetime for current datetime. |
| [start\_of\_the_month/1](#start_of_the_month1) | Provides Start of the Day information as datetime for the given datetime. |
| [start\_of\_the_week/0](#start_of_the_week0) | Provides Start of the Week information as datetime for current datetime. |
| [start\_of\_the_week/1](#start_of_the_week1) | Provides Start of the Week information as datetime for the given datetime. |
| [tomorrow/0](#tomorrow0) | Provides Tomorrow's information as datetime for current day. |
| [tomorrow/1](#tomorrow1) | Provides Tomorrow's information as datetime for the given datetime. |
| [total_seconds/1](#total_seconds1) | Provides total seconds by day, hour or minute. |
| [until_epoch/0](#until_epoch0) | Provides time spent since the AD. |
| [week_number/0](#week_number0) | Provides week number of current datetime. |
| [week_number/1](#week_number1) | Provides week number of the given datetime. |
| [yesterday/0](#yesterday0) | Provides Yesterday's information as datetime for current day. |
| [yesterday/1](#yesterday1) | Provides Yesterday's information as datetime for the given datetime. |

Function Details
----------------

### dayname_of_the_week/0
>dayname_of_the_week() -> {integer(), string(), string()}  
>`returns:` Day number starting from 1, long name of the day and short name of the day.  
>_Provides information of the current day in the current week in English._

### dayname_of_the_week/1
>dayname_of_the_week(DayNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> {integer(), string(), string()}  
>`returns:` Day number starting from 1, long name of the day and short name of the day.  
>_Provides information of the day for the given datetime or given day number in English._

### dayname_of_the_week/2
>dayname_of_the_week(DayNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), LangCode::atom()) -> {integer(), string(), string()}  
>`returns:` Day number starting from 1, long name of the day and short name of the day.  
>_Provides information of the current day for the given date time or given day number for the given ISO639-1 Language Code._

### days_between/2
>days_between(DateTimeFrom::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), DateTimeTo::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total days between two days as integer.  
>_Provides total difference in days between given two datetime. Second datetime's day is NOT counted within total days._

### days_from_now/1
>days_from_now(DateTimeTo::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total days between current datetime and given datetime as integer.  
>_Provides total difference in days between current datetime and given datetime. Given datetime's day is NOT counted within total days._

### days_since_begin_of_week/0
>days_since_begin_of_week() -> integer()  
>`returns:` Total days spent since the beginning of the week as integer().  
>_Provides total days spent since the begining of the week for current datetime. Current day is also counted within total days._

### days_since_begin_of_week/1
>days_since_begin_of_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total days spent since the beginning of the week as integer().  
>_Provides total days spent since the begining of the week for the given datetime. Given datetime's day is also counted within total days._

### days_since_begin_of_year/0
>days_since_begin_of_year() -> integer()  
>`returns:` Total days spent since the beginning of the year as integer().  
>_Provides total days spent since the begining of the year for current datetime. Current datetime is NOT counted within total days._

### days_since_begin_of_year/1
>days_since_begin_of_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total days spent since the beginning of the year as integer().  
>_Provides total days spent since the begining of the year for the given datetime. Current datetime is NOT counted within total days._

### days_until_end_of_week/0
>days_until_end_of_week() -> integer()  
>`returns:` Total days between current datetime and the end of current week as integer.  
>_Provides total days between current datetime and the end of the current week._

### days_until_end_of_week/1
>days_until_end_of_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total days between given datetime and the end of the given datetime's week as integer.  
>_Provides total days between given datetime and the end of the given datetime's week._

### days_until_end_of_year/0
>days_until_end_of_year() -> integer()  
>`returns:` Total days between current datetime and the end of the current datetime's year as integer.  
>_Provides total days between current datetime and the end of the current datetime's year._

### days_until_end_of_year/1
>days_until_end_of_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total days between given datetime and the end of the given datetime's year as integer.  
>_Provides total days between given datetime and the end of the given datetime's year._

### days_until_now/1
>days_until_now(DateTimeFrom::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total days between given datetime and current datetime as integer.  
>_Provides total difference in days between given datetime and current day. Current datetime's day is NOT counted within total days._

### end_of_the_day/0
>end_of_the_day() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Day including last second of the day as datetime.  
>_Provides End of the Day information as datetime for current datetime._

### end_of_the_day/1
>end_of_the_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Day including last second of the day as datetime.  
>_Provides End of the Day information as datetime for the given datetime._

### end_of_the_hour/0
>end_of_the_hour() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Hour including last second of the day as datetime.  
>_Provides End of the Hour information as datetime for current datetime._

### end_of_the_hour/1
>end_of_the_hour(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Hour including last second of the day as datetime.  
>_Provides End of the Hour information as datetime for the given datetime._

### end_of_the_minute/0
>end_of_the_minute() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Minute including last second of the day as datetime.  
>_Provides End of the Minute information as datetime for the current datetime._

### end_of_the_minute/1
>end_of_the_minute(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Minute including last second of the day as datetime.  
>_Provides End of the Minute information as datetime for the given datetime._

### end_of_the_month/0
>end_of_the_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Month including last second of the day as datetime.  
>_Provides End of the Month information as datetime for current datetime._

### end_of_the_month/1
>end_of_the_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Month including last second of the day as datetime.  
>_Provides End of the Month information as datetime for the given datetime._

### end_of_the_week/0
>end_of_the_week() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Week including last second of the day as datetime.  
>_Provides End of the Week information as datetime for current datetime._

### end_of_the_week/1
>end_of_the_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` End of the Week including last second of the day as datetime.  
>_Provides End of the Week information as datetime for the given datetime._

### epoch/0
>epoch() -> integer()  
>`returns:` Total seconds as integer.  
>_Provides Unix epoch as integer._

### epoch_start_date/0
>epoch_start_date() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Epoch start date in datetime.  
>_Provides Unix epoch start date information._

### format/2
>format(Format::string(), DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> string()  
>`returns:` Formatted datetime string as state in string formatters.  
>_Provides formatted reresentation of the given datetime by Ruby inspired date time formats._  


 	--- CONVERSION SPECIFIERS ---
  `%Y` - Year including century, zero-padded: 2022  
  `%y` - Year without century, in range (0.99), zero-padded: 22  
  `%C` - Century, zero-padded: 20  
  `%m` - Month of the year, in range (1..12), zero-padded: 01  
  `%B` - Full month name, capitalized: January  
  `%b` - Abbreviated month name, capitalized: Jan  
  `%h` - Same as %b.  
  `%d` - Day of the month, in range (1..31), zero-padded: 01  
  `%e` - Day of the month, in range (1..31), blank-padded: " 1”  
  `%j` - Day of the year, in range (1..366), zero-padded: 125  
  `%H` - Hour of the day, in range (0..23), zero-padded: 12  
  `%k` - Hour of the day, in range (0..23), blank-padded: " 1”  
  `%I` - Hour of the day, in range (1..12), zero-padded: 08  
  `%l` - Hour of the day, in range (1..12), blank-padded: " 8”  
  `%P` - Meridian indicator, lowercase: am, pm  
  `%p` - Meridian indicator, uppercase: AM, PM  
  `%M` - Minute of the hour, in range (0..59), zero-padded: 57  
  `%S` - Second of the minute in range (0..59), zero-padded: 18  
  `%s` - Number of seconds since the epoch: 1656505136  
  `%A` - Full weekday name: Wednesday  
  `%a` - Abbreviated weekday name: Wed  
  `%u` - Day of the week, in range (1..7), Monday is 1: 2  
  `%w` - Day of the week, in range (0..6), Sunday is 0: 0  
  `%W` - Week number of the year, in range (0..53), zero-padded, where each week begins on a Monday: 26  
 
  	--- SHORTHAND CONVERSION SPECIFIERS ---  
  `%c` - Date and time: "Wed Jun 29 08:01:41 2022", shorthand for '%a %b %e %T %Y’  
  `%D` - Date: "06/29/22", shorthand for ‘%m/%d/%y'  
  `%F` - ISO 8601 date: "2022-06-29", shorthand for ‘%Y-%m-%d'  
  `%v` - VMS date: "29-JUN-2022", shorthand for ‘%e-%^b-%Y'  
  `%x` - Same as %D  
  `%X` - Same as %T  
  `%r` - 12-hour time:  "01:00:00 AM", shorthand for '%I:%M:%S %p’  
  `%R` - 24-hour time: "01:00", shorthand for ‘%H:%M'  
  `%T` - 24-hour time: "01:00:00", shorthand for ‘%H:%M:%S'  
  `%+` - Date and time: "Wed Jun 29 08:31:53 -05:00 2022", shorthand for '%a %b %e %H:%M:%S %Z %Y’  

### last_month/0
>last_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Last Month as datetime.  
>_Provides Last Month's information as datetime for current datetime._

### last_month/1
>last_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Last Month as datetime.  
>_Provides Last Month's information as datetime for the given datetime._

### last_year/0
>last_year() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Last Year as datetime.  
>_Provides Last Year's information as datetime for current datetime._

### last_year/1
>last_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Last Year as datetime.  
>_Provides Last Year's information as datetime for the given datetime._

### middle_of_the_day/0
>middle_of_the_day() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Mid of the Day as datetime.  
>_Provides Mid of the Day information as datetime for the current datetime._

### middle_of_the_day/1
>middle_of_the_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Mid of the Day as datetime.  
>_Provides Mid of the Day information as datetime for the given datetime._

### month_name/0
>month_name() -> {integer(), string(), string()}  
>`returns:` Month numbers starting from 1, long month name, short month name.  
>_Provides month name for current datetime._

### month_name/1
>month_name(MonthNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> {integer(), string(), string()}  
>`returns:` Month numbers starting from 1, long month name, short month name.  
>_Provides month name for the given datetime or month number._

### month_name/2
>month_name(MonthNumber::integer() | [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), LangCode::atom()) -> {integer(), string(), string()}  
>`returns:` Month numbers starting from 1, long month name, short month name.  
>_Provides month name for the given datetime or month number for the given ISO639-1 Language code._

### next_month/0
>next_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Next Month as datetime.  
>_Provides Next Month's information as datetime for current datetime._

### next_month/1
>next_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Next Month as datetime.  
>_Provides Next Month's information as datetime for the given datetime._

### next_year/0
>next_year() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Next Year as datetime.  
>_Provides Next Year's information as datetime for current datetime._

### next_year/1
>next_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Next Year as datetime.  
>_Provides Next Year's information as datetime for the given datetime._

### seconds_between/2
>seconds_between(DateTimeFrom::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime), DateTimeTo::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total seconds between two days as integer.  
>_Provides total difference in seconds between given two datetime._

### seconds_since_begin_of_year/0
>seconds_since_begin_of_year() -> integer()  
>`returns:` Total seconds spent since the beginning of the year as integer.  
>_Provides Total seconds spent since the beginning of the year information as datetime for current datetime._

### seconds_since_begin_of_year/1
>seconds_since_begin_of_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total seconds spent since the beginning of the year as integer.  
>_Provides Total seconds spent since the beginning of the year information as datetime for the given datetime._

### seconds_since_midnight/0
>seconds_since_midnight() -> integer()  
>`returns:` Total seconds spent since the midnight as integer.  
>_Provides Total seconds spent since the midnight information as datetime for current datetime._

### seconds_since_midnight/1
>seconds_since_midnight(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total seconds spent since the midnight as integer.  
>_Provides Total seconds spent since the midnight information as datetime for the given datetime._

### seconds_until_end_of_day/0
>seconds_until_end_of_day() -> integer()  
>`returns:` Total seconds until the end of the current datetime as integer.  
>_Provides total seconds from current datetime and the end of current datetime's day._

### seconds_until_end_of_day/1
>seconds_until_end_of_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total seconds until the end of the given datetime as integer.  
>_Provides total seconds from given datetime and the end of given datetime's day._

### seconds_until_end_of_year/0
>seconds_until_end_of_year() -> integer()  
>`returns:` Total seconds until the end of the current datetime's last day in the year as integer.  
>_Provides total seconds from currnet datetime and the end of the current year._

### seconds_until_end_of_year/1
>seconds_until_end_of_year(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Total seconds until the end of the given datetime's last day as integer.  
>_Provides total seconds from given datetime and the end of given datetime's last day in its year._

### start_of_the_day/0
>start_of_the_day() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Day as datetime.  
>_Provides Start of the Day information as datetime for current datetime._

### start_of_the_day/1
>start_of_the_day(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Day as datetime.  
>_Provides Start of the Day information as datetime for the given datetime._

### start_of_the_hour/0
>start_of_the_hour() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Hour as datetime.  
>_Provides Start of the Hour information as datetime for current datetime._

### start_of_the_hour/1
>start_of_the_hour(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Hour as datetime.  
>_Provides Start of the Hour information as datetime for the given datetime._

### start_of_the_minute/0
>start_of_the_minute() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Minute as datetime.  
>_Provides Start of the Minute information as datetime for current datetime._

### start_of_the_minute/1
>start_of_the_minute(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Minute as datetime.  
>_Provides Start of the Minute information as datetime for the given datetime._

### start_of_the_month/0
>start_of_the_month() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Day as datetime.  
>_Provides Start of the Day information as datetime for current datetime._

### start_of_the_month/1
>start_of_the_month(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Day as datetime.  
>_Provides Start of the Day information as datetime for the given datetime._

### start_of_the_week/0
>start_of_the_week() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Week as datetime.  
>_Provides Start of the Week information as datetime for current datetime._

### start_of_the_week/1
>start_of_the_week(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Start of the Week as datetime.  
>_Provides Start of the Week information as datetime for the given datetime._

### tomorrow/0
>tomorrow() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Tomorrow as datetime.  
>_Provides Tomorrow's information as datetime for current day._

### tomorrow/1
>tomorrow(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Tomorrow as datetime.  
>_Provides Tomorrow's information as datetime for the given datetime._

### total_seconds/1
>total_seconds(X1::atom()) -> integer()  
>`returns:` Total seconds as integer.  
>_Provides total seconds by day, hour or minute._

### until_epoch/0
>until_epoch() -> integer()  
>`returns:` Total seconds in between AD and Unix epoch.  
>_Provides time spent since the AD._

### week_number/0
>week_number() -> integer()  
>`returns:` Week number of current datetime as integer.  
>_Provides week number of current datetime._

### week_number/1
>week_number(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> integer()  
>`returns:` Week number of the given datetime as integer.  
>_Provides week number of the given datetime._

### yesterday/0
>yesterday() -> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Yesterday as datetime.  
>_Provides Yesterday's information as datetime for current day._

### yesterday/1
>yesterday(DateTime::[calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)) -\> [calendar:datetime()](http://www.erlang.org/edoc/doc/stdlib/doc/calendar.html#type-datetime)  
>`returns:` Yesterday as datetime.  
>_Provides Yesterday's information as datetime for the given datetime._
