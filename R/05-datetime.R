#' 将字符串转化为时分秒
#'
#' @param x 字符串
#'
#' @return 返回值
#' @import hms
#' @export
#'
#' @examples
#' hms();
hms <- function(x) {
res <-  as.hms(x) ; 
return(res);
  
}

#' 判断时间是否为凌晨
#'
#' @param x  时间
#' @param morningStartTime 凌晨开始时间
#' @param morningEndTime   凌晨结束时间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' is.Morning();
is.Morning <- function(x,morningStartTime='0:00:00',morningEndTime='6:00:00') {
  if (class(x) == 'character'){
    x <- hms(x);
  }
  morningStartTime <- hms(morningStartTime);
  morningEndTime <- hms(morningEndTime);
  res <- x> morningStartTime & x < morningEndTime;
  return(res);
  
}

#' 判断时间字段是否为上午
#'
#' @param AmEndTime 上午结束时间
#' @param x  时间字段
#'
#' @return 返回值
#' @export
#'
#' @examples
#' is.Am(9:00:00);
is.Am <- function(x,AmStartTime='6:00:00',AmEndTime='12:00:00') {
  if (class(x) == 'character'){
    x <- hms(x);
  }
  AmStartTime <- hms(AmStartTime);
  AmEndTime <- hms(AmEndTime);
  res <- x> AmStartTime & x < AmEndTime;
  return(res);
}
# 判断时间是否为下午时间----
#' 判断时间是否为下午时间
#'
#' @param x 日期范围
#' @param startTime 下午开始时间
#' @param endTime   下午结束时间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' is.Pm('14:00:00');
is.Pm <- function(x,startTime='12:00:00',endTime="18:30:00") {
  if (class(x) == 'character'){
    x <- hms(x);
  }
  startTime <- hms(startTime);
  endTime <- hms(endTime);
  res <- x >startTime & x < endTime
  return(res);
  
}

#' 判断是否为夜晚
#'
#' @param x  数值
#' @param nightStartTime 夜晚开始时间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' is.Night('19:00:00');
is.Night <- function(x,nightStartTime='18:30:00') {
  if (class(x) == 'character'){
    x <- hms(x);
  }
  nightStartTime <- hms(nightStartTime);
  res <- x > nightStartTime;
  return(res);
  
}


#' 处理两个时间差
#'
#' @param x 原始时间数据
#' @param targetTime  目标时间数据
#' @param unit 差额的时间单位
#' @param digit 保留小数位数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' timeDiff('6:00:00','9:00:00')
timeDiff <- function(x,targetTime,unit='h',digit=1) {
  if ( class(x) =='character'){
    x <- hms(x);
  }
  if (class(targetTime) == 'character'){
    targetTime <- hms(targetTime);
  }
  
  #cof <- 1;
  
  if (unit == 'h'){
    cof <-3600;
  } else  if(unit == 'm'){
   cof <- 60; 
  } else {
    cof <-1;
  }
   res <- round(as.numeric(targetTime-x)/cof,digit);
   return(res);
}

#' 将字符串变成完整的时间
#'
#' @param x 字符串
#'
#' @return 返回值
#' @export
#'
#' @examples
#' as.datetime('2019-08-14 08:00:00')
as.datetime <- function(x) {
  res <- as.POSIXct(x);
  return(res);
  
}

#' 判断对象是否为日期时间型
#'
#' @param x 对象 
#'
#' @return 返回值
#' @export
#'
#' @examples
#' is.datetime(as.datetime('2019-08-14 08:14:00'));
is.datetime <- function(x){
 res <- "POSIXct" %in% class(x);
 return(res);
}

#' 计算出两个datetime类型数据的时间差
#'
#' @param x  第一个数据
#' @param y  第一个数据
#' @param unit 时间差单位，默认为h表示小时
#' @param digit 保留小数位数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' datetime_diff(as.datetime('2019-08-14 08:14:00'),as.datetime('2019-08-15 08:14:00'))
datetime_diff <- function(x,y,unit='h',digit=1)
{
  if(class(x) == 'character'){
    x <- as.datetime(x);
  }
  if(class(y) == 'character'){
    y <- as.datetime(y)
  }
  cof <-1;
  if (unit == 'h'){
    cof <-3600;
  }else if(unit == 'm'){
    cof <-60;
  }else{
    cof <-1;
  }
  res <- round((as.numeric(y)-as.numeric(x))/cof,digit);
  return(res);
}

#' 处理日期字段，兼容as.Date
#'
#' @param x 文本
#'
#' @return 返回日期字段
#' @export
#'
#' @examples
#' as.date('2019-04-09');
as.date <- function(x) {
  if (class(x) == 'character'){
    res <- as.Date(x);
  }else{
    res <- x;
  }
   
  return(res);
  
}

#' 判断是否为日期类型的字段
#'
#' @param x  对象
#'
#' @return 返回值
#' @export
#'
#' @examples
#' is.date(as.date('2019-09-09'));
is.date <- function(x) {
  res <- class(x) =="Date";
  return(res);
  
}

#' 计算2个日期之间的差异
#'
#' @param startDate 开始日期
#' @param endDate  结束日期
#' @param unit    日期差异的单位,默认为d
#' @param hourPerDay  每天工作小时数
#' @param is.hr  是否HR计时
#'
#' @return 返回值
#' @export
#'
#' @examples
#' date_diff('2019-6-11','2019-07-22')
date_diff <- function(startDate,endDate,unit='d',hourPerDay=8,is.hr=TRUE) {
  if (class(startDate) == 'character'){
    startDate <- as.date(startDate)
  }
  if(class(endDate) == 'character'){
    endDate <- as.date(endDate);
  }
  offset1 <- 0;
  if (is.hr == TRUE ){
    offset1 <- 1; 
  }
  if (unit == 'd'){
    hourPerDay <-1;
  }
  res <- (as.numeric(endDate)-as.numeric(startDate)+offset1)* hourPerDay;
  return(res);
  
  
}



#' 获取当前时间
#'
#' @param n 重复次数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getTime()
getTime <- function(n=1) {
  r <- as.character(Sys.time())
  res <- rep(r,n)
  return(res)
  
}
