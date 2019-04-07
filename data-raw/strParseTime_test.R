library(tsda);
x <-datetimeInput('2018-12-12 12:12:12');
x;
x+1:10000;
 "POSIXct" %in% class(x);

Sys.time()+1;
getTimeSeries('123213213213',2);

month.abb;
month.name;

library(xts);
x<-1:10;
y<-11:20;
z <- getDateSeries(days = 10);
z;
xts1 <-xts(z,x,y);
data1 <-data.frame(x,y);
data1 <- as.matrix(data1);
data1;
xts2 <- xts(data1,z);
lag(xts2);

?? strptime
