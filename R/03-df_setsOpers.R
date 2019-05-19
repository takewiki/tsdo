#' 将data存在于data_frame1而不存在于data_frame2的记录取出
#'
#' @param data_frame1 第一个数据框
#' @param data_frame2 第二个数据框
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples 
#' library(tsdo);
#' data('setExample1')
#' data('setExample2')
#' df_setdiff(setExample1,setExample2);
df_setdiff <-function(data_frame1,data_frame2){
  setdiff(data_frame1,data_frame2);
}



#' 取两个数据框的交集
#'
#' @param data_frame1 第一个数据框
#' @param data_frame2 第二个数据框
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples 
#' library(tsdo);
#' data('setExample1')
#' data('setExample2')
#' df_intersect(setExample1,setExample2);
df_intersect <- function(data_frame1,data_frame2) {
  intersect(data_frame1,data_frame2);
  
  
}



#' 取2个数据框的并集
#'
#' @param data_frame1 第一个数据框
#' @param data_frame2 第二个数据框
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' library(tsdo);
#' data('setExample1')
#' data('setExample2')
#' df_union(setExample1,setExample2);
df_union <- function(data_frame1,data_frame2) {
  union(data_frame1,data_frame2);
  
}
