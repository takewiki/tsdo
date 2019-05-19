#' 获取数据框每个分组前N个数据
#'
#' @param data 数据框
#' @param gp_fieldName 分组字段，支持组合
#' @param order_fieldName 排序字段
#' @param N 前N个数据
#'
#' @return 返回数据框
#' @export
#'
#' @examples  df_groupBy_topN();
#' df_groupBy_topN(data = iris,gp_fieldName = 'Species',order_fieldName = 'Sepal.Width',N = 2
#'
#');
df_groupBy_topN <- function(data=iris,gp_fieldName=c('Species','Petal.Width'),order_fieldName='Sepal.Width',N=3){
  data <- substitute(data);
  part1 <- paste(gp_fieldName,collapse = ",");
  expr <- paste('res <- ',data,' %>% group_by(',part1,') %>%
    top_n(', N,',' ,order_fieldName,');');
  expr <- R_expr(expr);
  R_exec(expr);
  res <- as.data.frame(res,stringsAsFactors=F);
  return(res)
  
  
}




