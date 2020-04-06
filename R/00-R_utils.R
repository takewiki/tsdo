#' 将字符串形成符合R要求的表达式
#'
#' @param string 字符串
#'
#' @return 返回值
#' @export
#'
#' @examples R_expr()
R_expr <- function(string){
  res <- parse(text = string);
  return(res)
}

#' 执行R代表
#'
#' @param expr 表达式
#'
#' @return 执行结果，不需要返回
#' @export
#'
#' @examples R_exec();
R_exec <- function(expr){
  eval(expr = expr,envir = parent.frame())
}
# 引入相应的管道符号
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`





#' 添加下载函数
#'
#' @param src 文件来源
#' @param dest 目标
#'
#' @return 返回值
#' @export
#'
#' @examples
#' download()
download <- function(src,dest) {
  r <- download.file(src,dest,quiet = TRUE)
  if(r == 0){
    res <-TRUE
  }else{
    res <- FALSE
  }
  return(res)
  
}
