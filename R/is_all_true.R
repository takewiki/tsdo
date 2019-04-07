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
  if (F %in% x)
  {
    F
  }else{
    T
  }
}
