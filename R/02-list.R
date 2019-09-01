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



#' 处理初始化list操作
#'
#' @param len 元素个数
#' @param listNames 元素名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' library(tsdo);
#' aa <-list_init(3);
#' aa;
#' bb<-list_init(listNames = LETTERS);
#' bb;
#' 
list_init <- function(len=3,listNames=NULL) {
  if(is.null(listNames)){
    res <- vector('list',len)
}else{
  len <- length(listNames);
  res <- vector('list',len);
  names(res) <- listNames;
}
return(res);
  
}  