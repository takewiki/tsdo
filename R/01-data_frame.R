#  返回数据值的每列的数据类型 ----
#' 返回数据值的每列的数据类型 
#'
#' @param data_frame 数据框
#'
#' @return 返回值
#' @export
#'
#' @examples 
df_columnType <- function(data_frame) {
  if (class(data_frame) != 'data.frame'){
    stop('数据类型必须为data.frame！',call. = FALSE)
  }
  res <- unlist(lapply(data_frame, class));
  return(res);
}

#' 获取数据框的列名
#'
#' @param data_frame 数据框
#'
#' @return 返回值
#' @export
#'
#' @examples df_columnName()
df_columnName <- function(data_frame) {
  if (class(data_frame) != 'data.frame'){
    stop('数据类型必须为data.frame！',call. = FALSE)
  }
  res <- names(data_frame)
  return(res);
}

