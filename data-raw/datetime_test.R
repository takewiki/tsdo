library(tsda);
datetimeInput('2019-01-12 12:30:49') ->date5;
date5;
class(date5);
?? with_tz
Sys.timezone(location = T);
?? OlsonNames

bb<-datetime.LocalPrint(datetime('2018-02-16 12:31:47'),'PRC');
bb;
cc <-datetime('2018-02-16 12:31:47');
cc;
bb ==cc;
dd <- force_tz(cc,'PRC');
dd==cc;


library(tsda);
bb <-timeZoneNames();
bb;
class(bb);
tt <-OlsonNames()
class(tt);
