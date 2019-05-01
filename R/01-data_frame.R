#' 返回数据值的每列的 
#'
#' @param data_frame 数据框
#'
#' @return 返回值
#' @export
#'
#' @examples 
df_columnType <- function(data_frame) {
  res <- unlist(lapply(tensorFrameExample, class));
  return(res);
  
}
