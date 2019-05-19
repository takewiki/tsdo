#' 定义逻辑判断方案，选择第一个方案或备用方案
#'
#' @param a 第一个方案
#' @param b 备用方案
#'
#' @return 返回值
#' @export
#'
#' @examples 'a' %||% 'b'
`%||%` <- function(a,b){
  if (!is.na(a)){
    res <- a
  }else{
    res <- b
  }
  return(res)
}


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

#' 判断逻辑向量是否全部为true
#'
#' @param x 逻辑向量
#'
#' @return  返回T or F
#' @export
#'
#' @examples  is_all_true(T,T,T,F)
is_all_true <-function (x)
{
  if (NA %in% x)
  {
    NA
  }else if( FALSE %in% x){
    FALSE
  }else{
    TRUE
  }
}


#' 判断奇偶数，如果是奇数返回T，否则返回F
#'
#' @param x 整体型的数组变量
#'
#' @return 返回一个逻辑向量
#' @export
#'
#' @examples is.odd(1:100)
is.odd <- function(x){
  ifelse(x%%2 ==0,F,T)
}
