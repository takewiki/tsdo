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