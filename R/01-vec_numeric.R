#' 如果数值小于等于某个目标值进行替代为新值
#'
#' @param x 原始数值向量
#' @param targetValue 目标值
#' @param replaceValue 替代值默认为0
#'
#' @return 返回值
#' @export
#'
#' @examples
#' replaceValue_lessThan_targetv();
replaceValue_lessThan_target <- function(x,targetValue=15,replaceValue=0) {
  x[x<= targetValue] <-replaceValue;
  return(x);
  
  
}

