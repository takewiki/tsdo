# 将数据框进行分组计算----
# 这是在R语言中进行处理，如果如果量较大，还是要将数据保存到数据
# 然后使用select group by进行处理
#' 将数据框进行分组计算
#'
#' @param data 数据框
#' @param gp_fieldName 分组变量字段
#' @param gp_fieldCaption 分组变量标题
#' @param summ_fieldName 聚合变量字段
#' @param funName 函数名称
#' @param summ_fieldCaption 聚会字段标题
#'
#' @return 返回数据框
#' @import dplyr
#' @export
#'
#' @examples df_groupBy_summarise_cols_funs();
#' df_groupBy_summarise_cols_funs(mtcars,
#' gp_fieldName = c('mpg','cyl','disp'),
#' gp_fieldCaption = c('gp1','gp2','gp3'),
#' funName = c('sum','sum','sum'),
#' summ_fieldCaption = c('sum1','sum2','sum3'));
df_groupBy_summarise_cols_funs <- function(data = mtcars,gp_fieldName=c('cyl','disp'),gp_fieldCaption=NULL, summ_fieldName = c("mpg", "cyl",
                                                    "disp"), funName = c("mean", "sum", "length"),
                       summ_fieldCaption = letters[1:3]){
  
  if(is.null(gp_fieldCaption)){
    gp_fieldCaption <-gp_fieldName;
  }
  
  data <- substitute(data);
  part2 <- paste(summ_fieldCaption,"=",funName,"(",summ_fieldName,")",sep="",collapse = ",");
  part1<-paste(gp_fieldName,collapse = ",");
  expr <-paste('res <- ',data," %>% group_by(",part1,") %>% summarise(",part2,');',sep = "")
  expr <-R_expr(expr);
  R_exec(expr);
  res <- as.data.frame(res,stringAsFactors=F);
  names(res) <-c(gp_fieldCaption,summ_fieldCaption);
  return(res);
}