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
is.Am <- function(x,AmEndTime='12:00:00') {
  if (class(x) == 'character'){
    x <- hms(x);
  }
  startTime <- hms(startTime);
  res <- x < startTime;
  return(res);
}