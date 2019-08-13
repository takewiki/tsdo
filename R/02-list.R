#' 将列表转化为数据框
#'
#' @param data 列表数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' listAsDF_cols();
listAsDF_cols <- function(data) {
  res <- as.data.frame(data,stringsAsFactors=FALSE);
  return(res);
  
}
#' 将列表转化为数据框
#'
#' @param data 列表数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' listAsDF();
listAsDF <- function(data) {
  res <- as.data.frame(data,stringsAsFactors=FALSE);
  return(res);
  
}

#' 将列表数据按行合并为DF
#'
#' @param data list数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' listAsDF_rows();
listAsDF_rows <- function(data) {
  res <- do.call('rbind',data);
  return(res);
  
}