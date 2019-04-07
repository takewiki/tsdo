library(tsda);
cc <-tsda::ymd(20150112);
class(cc);
yyyymmdd(20180125);

mmddyyyy("04/01/2017");

ddmmyyyy('29/04/1984');


time <- ymd_hms("2010-12-13 15:30:30")
time

class(time);

#测试数据
library(tsda);
date1 <-'1984.04.09';
date1 <- yyyymmdd(date1,'.');
date1;

#测试一下数字
date2 <-20180125;
date2 <-yyyymmdd(date2,'N');
date2;
#测试一下分隔符
date3 <-'2018/04/09';
date3 <- yyyymmdd(date3,'/');
date3;

#测试mdy
(mmddyyyy('04-09-2018','-'));
(mmddyyyy('04.09.2018','.'));
(mmddyyyy('04/09/2018','/'));

#测试mdy
(ddmmyyyy('04-09-2018','-'));
(ddmmyyyy('04.09.2018','.'));
(ddmmyyyy('04/09/2018','/'));

date3 <-c('2018-01-31','2018-02-21','2018-03-21');
date4 <-(yyyymmdd(date3));
class(date4);
date1+1:30000;
-10:-1 +1;







