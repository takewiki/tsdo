#' 处理日期数据
#'
#' @param startDate  开始日期 
#' @param length   个数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' date_add();
date_add <- function(startDate='1984-01-01',length=5) {
  #针对数据类型进行处理
  startDate <- as.date(startDate);
  #处理数据，包含当天数据，用于HR处理
  index <- 1:length -1;
  #生成序列
  res <- startDate+index;
  return(res);
  
}

#' 生成日期序列
#'
#' @param startDate  开始日志
#' @param endDate  结束日期
#'
#' @return 返回值
#' @export 
#'
#' @examples
#' date_minus();
date_minus <-function(startDate='1984-01-01',endDate='1984-01-05'){
  #针对数据类型进行处理
  startDate <- as.date(startDate);
  endDate <- as.date(endDate);
  indexLen <- endDate-startDate;
  res <- startDate+ 0:indexLen;
  return(res);
  
  
}
  