library(tsda);
#直接测试
getDateSeries();

#测试其他数据
getDateSeries(days = 300);
#测试1984数据

getDateSeries(as.Date('1094-01-01'),100);

#测试负数
getDateSeries(as.Date('1094-01-01'),-100);

#最近七天
date1 <-getDateSeries(days = 7);
date2 <-getDateSeries(as.Date('1094-01-01'),7);
duration <-date1 -date2;
str(duration);
class(duration);
