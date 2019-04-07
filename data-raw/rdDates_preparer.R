library(tsda);
rdDates_file <- './data-raw/rdDates.xlsx';
class(rdDates_file) <-'excel';
rdDates <-readData(rdDates_file);
rdDates$Fdate <- as.Date(rdDates$Fdate);
#rdDates$FdateStr <- as.character(rdDates$Fdate);
#View(rdDates);
devtools::use_data(rdDates,internal = T,overwrite = T);
#str(rdDates);
#记录日期数据
