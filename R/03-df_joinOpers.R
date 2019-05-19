#' 将两个数据框进行左连接，连接字段名相同
#'
#' @param data_frame1 第1数据框
#' @param data_fram2  第2数据框
#' @param byFieldNames 连接字段名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' library(tsdo);
#' data("joinData1");
#' data("joinData2");
#' filed_by <-c('fname','fclass');
#' df_leftJoin_bySameColNames(joinData1,joinData2,filed_by); 
df_leftJoin_bySameColNames <- function(data_frame1,data_fram2,byFieldNames){
  left_join(data_frame1,data_fram2,byFieldNames)
}

#' 两个数据框左连接，列名不同，但一一对应
#'
#' @param data_frame1 第1数据框
#' @param data_frame2 每2数据框
#' @param leftBy_fieldName 左列名
#' @param rightBy_fieldName 右列表
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' library(tsdo);
#' data("joinData1");
#' data("joinData3");
#' left_by <-c('fname','fclass');
#' right_by <-c('titileName','titleClass');
#' df_leftJoin_byDiffColNames(joinData1,joinData3,left_by,right_by);
df_leftJoin_byDiffColNames <- function(data_frame1,data_frame2,leftBy_fieldName,rightBy_fieldName){
  df1 <- substitute(data_frame1);
  df2 <- substitute(data_frame2);
  part1 <- paste("'",leftBy_fieldName,"' = '",rightBy_fieldName,"'",sep="",collapse = ",");
  expr <- paste(
    
    'res <-left_join(',df1,',',df2,',by=c(',part1,'));',sep=""
  )
  
  #return(expr);
  expr <-R_expr(expr);
  R_exec(expr);
  res <- tbl_as_df(res);
  return(res);
}

#' 将两个数据框进行右连接，连接字段名相同
#'
#' @param data_frame1 第1数据框
#' @param data_fram2  第2数据框
#' @param byFieldNames 连接字段名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' library(tsdo);
#'data("joinData1");
#' data("joinData2");
#' filed_by <-c('fname','fclass');
#' df_rightJoin_bySameColNames(joinData1,joinData2,filed_by)
df_rightJoin_bySameColNames <- function(data_frame1,data_fram2,byFieldNames){
  right_join(data_frame1,data_fram2,byFieldNames)
}

#' 两个数据框右连接，列名不同，但一一对应
#'
#' @param data_frame1 第1数据框
#' @param data_frame2 每2数据框
#' @param leftBy_fieldName 左列名
#' @param rightBy_fieldName 右列表
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' library(tsdo);
#' data("joinData1");
#' data("joinData3");
#' left_by <-c('fname','fclass');
#' right_by <-c('titileName','titleClass');
#' df_rightJoin_byDiffColNames(joinData1,joinData3,left_by,right_by);
df_rightJoin_byDiffColNames <- function(data_frame1,data_frame2,leftBy_fieldName,rightBy_fieldName){
  df1 <- substitute(data_frame1);
  df2 <- substitute(data_frame2);
  part1 <- paste("'",leftBy_fieldName,"' = '",rightBy_fieldName,"'",sep="",collapse = ",");
  expr <- paste(
    
    'res <-right_join(',df1,',',df2,',by=c(',part1,'));',sep=""
  )
  
  #return(expr);
  expr <-R_expr(expr);
  R_exec(expr);
  res <- tbl_as_df(res);
  return(res);
}

#' 将两个数据框进行内连接，连接字段名相同
#'
#' @param data_frame1 第1数据框
#' @param data_fram2  第2数据框
#' @param byFieldNames 连接字段名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' library(tsdo);
#' data("joinData1");
#' data("joinData2");
#' filed_by <-c('fname','fclass');
#' df_innerJoin_bySameColNames(joinData1,joinData2,filed_by) 
df_innerJoin_bySameColNames <- function(data_frame1,data_fram2,byFieldNames){
  inner_join(data_frame1,data_fram2,byFieldNames)
}

#' 两个数据框内连接，列名不同，但一一对应
#'
#' @param data_frame1 第1数据框
#' @param data_frame2 每2数据框
#' @param leftBy_fieldName 左列名
#' @param rightBy_fieldName 右列表
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' library(tsdo);
#' data("joinData1");
#' data("joinData3");
#' left_by <-c('fname','fclass');
#' right_by <-c('titileName','titleClass');
#' df_innerJoin_byDiffColNames(joinData1,joinData3,left_by,right_by);
df_innerJoin_byDiffColNames <- function(data_frame1,data_frame2,leftBy_fieldName,rightBy_fieldName){
  df1 <- substitute(data_frame1);
  df2 <- substitute(data_frame2);
  part1 <- paste("'",leftBy_fieldName,"' = '",rightBy_fieldName,"'",sep="",collapse = ",");
  expr <- paste(
    
    'res <-inner_join(',df1,',',df2,',by=c(',part1,'));',sep=""
  )
  
  #return(expr);
  expr <-R_expr(expr);
  R_exec(expr);
  res <- tbl_as_df(res);
  return(res);
}

#' 将两个数据框进行全连接，连接字段名相同
#'
#' @param data_frame1 第1数据框
#' @param data_fram2  第2数据框
#' @param byFieldNames 连接字段名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' library(tsdo);
#' data("joinData1");
#' data("joinData2");
#' filed_by <-c('fname','fclass');
#' df_fullJoin_bySameColNames(joinData1,joinData2,filed_by)
df_fullJoin_bySameColNames <- function(data_frame1,data_fram2,byFieldNames){
  full_join(data_frame1,data_fram2,byFieldNames)
}

#' 两个数据框全连接，列名不同，但一一对应
#'
#' @param data_frame1 第1数据框
#' @param data_frame2 每2数据框
#' @param leftBy_fieldName 左列名
#' @param rightBy_fieldName 右列表
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' library(tsdo);
#' data("joinData1");
#' data("joinData3");
#' left_by <-c('fname','fclass');
#' right_by <-c('titileName','titleClass');
#' df_fullJoin_byDiffColNames(joinData1,joinData3,left_by,right_by);
df_fullJoin_byDiffColNames <- function(data_frame1,data_frame2,leftBy_fieldName,rightBy_fieldName){
  df1 <- substitute(data_frame1);
  df2 <- substitute(data_frame2);
  part1 <- paste("'",leftBy_fieldName,"' = '",rightBy_fieldName,"'",sep="",collapse = ",");
  expr <- paste(
    
    'res <-full_join(',df1,',',df2,',by=c(',part1,'));',sep=""
  )
  
  #return(expr);
  expr <-R_expr(expr);
  R_exec(expr);
  res <- tbl_as_df(res);
  return(res);
}