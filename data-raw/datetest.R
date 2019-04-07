duration(day = -1)
# -86400s (~-1 days)
duration(90, "seconds")
duration(1.5, "minutes")
duration(-1, "days")
# -86400s (~-1 days)
duration(second = 90)
duration(minute = 1.5)
duration(mins = 1.5)
duration(second = 3, minute = 1.5, hour = 2, day = 6, week = 1)
duration(hour = 1, minute = -60)
duration("2M 1sec")
duration("2hours 2minutes 1second")
duration("2d 2H 2M 2S")
duration("2days 2hours 2mins 2secs")
# Missing numerals default to 1. Repeated units are added up.
duration("day day")
# Comparison with characters is supported from v1.6.0.
duration("day 2 sec") > "day 1sec"

tt <-dminutes(100);
class(tt);

?? year

duration(day = -1)
# -86400s (~-1 days)
duration(90, "seconds")
duration(1.5, "minutes")
duration(-1, "days")
# -86400s (~-1 days)
duration(second = 90)
duration(minute = 1.5)
duration(mins = 1.5)
duration(second = 3, minute = 1.5, hour = 2, day = 6, week = 1)
duration(hour = 1, minute = -60)
duration("2M 1sec")
duration("2hours 2minutes 1second")
duration("2d 2H 2M 2S")
duration("2days 2hours 2mins 2secs")
# Missing numerals default to 1. Repeated units are added up.
duration("day day")
# Comparison with characters is supported from v1.6.0.
duration("day 2 sec") > "day 1sec"



x <- ymd("2012-03-26")
year(x)

year
year(x) <- 2001;
x;

isS4(year)
isS3method('year')
isS3stdGeneric('year');

tsda::year(x) <-1231;
x;
tsda::yea
