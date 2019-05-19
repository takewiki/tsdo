#' 将tbl转化为df
#'
#' @param tbl  tbl
#'
#' @return 返回值
#' @export
#'
#' @examples tbl_as_df
tbl_as_df <- function(tbl){
  as.data.frame(tbl,stringAsFactors=F)
}