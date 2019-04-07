
#' 如果向量值为NA，则替代为相应的replace_value
#'
#' @param x  定义一个原生的向量，可以是数值或签字串
#' @param replace_value  定义一个替代值
#'
#' @return  返回一个向量
#' @export
#'
#' @examples  value_if_na(c(1,2,3,NA,NA,4),5);
value_if_na <- function(x,replace_value)
{
  x[is.na(x)] <-replace_value;
  x;
}
