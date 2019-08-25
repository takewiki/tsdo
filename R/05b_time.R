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





