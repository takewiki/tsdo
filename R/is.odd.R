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
