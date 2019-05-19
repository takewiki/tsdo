# 针对表中的字段进行统计，在不分组的情况下-----
#' 针对表中的字段进行统计，在不分的情况下
#'
#' @param fieldName 字段名称
#' @param funName 函数名称
#' @param fieldCaption  字段标题
#' @param data 数据集
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples df_summarise_cols_funs();
#' df_summarise_cols_funs(data=mtcars,
#' fieldName=c('mpg','cyl','disp'),
#' funName=c('mean','sum','length'),
#' fieldCaption=letters[1:3]
#' );
df_summarise_cols_funs <- function(data=mtcars,fieldName=c('mpg','cyl','disp'),funName=c('mean','sum','length'),fieldCaption=letters[1:3]){
  
  
  data <- substitute(data);
  part1 <- paste(fieldCaption,"=",funName,"(",fieldName,")",sep="",collapse = ",");
  expr <-paste('res <- summarise(',data,",", part1,');',sep = "")
  expr <-R_expr(expr);
  R_exec(expr);
  return(res);
}

#' 表的指定列使用同一个函数处理
#'
#' @param data 数据框
#' @param fieldName 字段名
#' @param fun 函数
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples df_summarise_cols_oneFun();
#' df_summarise_cols_oneFun(data=mtcars,
#' fieldName=c('mpg','cyl'),length);
df_summarise_cols_oneFun <-function(data=mtcars,fieldName=c('mpg','cyl','disp'),fun){
  
  data <- substitute(data);
  fun <- substitute(fun);
  expr<-paste('res <- summarise_at(.tbl =',data,",fieldName,",fun,");")
  expr <-R_expr(expr);
  R_exec(expr);
  return(res);
  
}

#' 针对数据框所有列使用同一个函数处理
#'
#' @param data 
#' @param fun 函数名，只指定一个
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples  df_summarise_allCols_oneFun();
df_summarise_allCols_oneFun<-function(data,fun){
  data <- substitute(data);
  fun <- substitute(fun);
  expr <-paste('res <- summarise_all(',data,",", fun,');',sep = "")
  expr <-R_expr(expr);
  R_exec(expr);
  return(res);
}

# 进行表级的计数，可以多列一起进行计数-----
#' 进行表级的计数，可以多列一起进行计数
#'
#' @param data 数据框
#' @param fieldName 字名名
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples df_count();
#' df_count(data=mtcars,
#' fieldName=c('mpg','cyl','disp')
#' );
df_count <- function(data,fieldName=c('mpg','cyl','disp')) {
  data <- substitute(data);
  part1 <-paste(fieldName,collapse = ",");
  expr <-paste('res <- count(',data,",", part1,');',sep = "")
  expr <-R_expr(expr);
  R_exec(expr);
  res <-as.data.frame(res,stringAsFactors=F);
  names(res) <- c(fieldName,'count');
  return(res);
  
}


mtcars %>%
  group_by(cyl) %>%
  summarise(avg = mean(mpg),sum=sum(disp),total=sum(mpg));

