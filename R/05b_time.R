#' 针对日期数据进行取舍
#'
#' @param x 原始数据，完整时间类型的字符向量
#' @param unit 四舍五入的单位
#'
#' @return 返回值
#' @import hms
#' @export
#'
#' @examples
#' round_time('9:12:05','min');
round_time <- function(x,unit=c('hour','min')) {

res <- as.hms(x);
cof <- 1;

if (unit == 'hour')
{
  cof <- 3600
}else if (unit == 'min'){
  cof <-60
}else{
  cof <-1
}

res <-trunc_hms(res,cof);
res <- as.character(res);
return(res);
}



#' 格式化时间部分
#'
#' @param value 值
#' @param unit 单位
#'
#' @return 返回值
#' @export
#'
#' @examples
#' time_formatter()
time_formatter <- function(value="0",unit='年'){
  if(value == '0')
  {
    res <-""
  }else{
    res <- paste0(as.character(value),unit)
  }
  return(res)
}
  
#' 格式化秒数
#'
#' @param seconds 整数型的秒数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' seconds_as——TimeText()
seconds_as_Timetext <- function(seconds){
  
  value <- lubridate::seconds_to_period(seconds)
  value_year <- as.character(value@year)
  value_month <- as.character(value@month)
  value_day <- as.character(value@day)
  value_hour <- as.character(value@hour)
  value_minute <- as.character(value@minute)
  value_second <- as.character(value@.Data)
  
  part_year <- time_formatter(value_year,'年')
  part_month <-time_formatter(value_month,'月')
  part_day <- time_formatter(value_day,'日')
  part_hour <- time_formatter(value_hour,'小时')
  part_minute <- time_formatter(value_minute,'分')
  part_second <- time_formatter(value_second,'秒')
  res <- paste0(part_year,part_month,part_day,part_hour,part_minute,part_second)
  return(res)
  

  
}

library(lubridate)

bb<- seconds_to_period(80)

as.character(bb@year) =="0"

