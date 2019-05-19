
#' 针对逻辑值转化为符合R语言规范的规格 
#'
#' @param x 数据
#'
#' @return 返回值
#'
#' @examples comboLogi_to_r();
comboLogi_to_r <- function(x){
  if (x == 'and'){
    res <-'&'
  }else if(x =='or'){
    res <-"|"
  }else{
    res <-x
  }
  return(res)
}
#' 将and or转化为R语言中的转换符
#'
#' @param x  充满and or的字符串
#'
#' @return 返回R中的比较符
#' @export
#'
#' @examples comboLogi_to_rs();
comboLogi_to_rs <- function(x){
  lapply(x, comboLogi_to_r) %>% unlist
}

#' 处理等于号由单个变成双个变成比较而不是赋值的处理
#'
#' @param x 数据
#'
#' @return 返回值
#'
#' @examples equal_to_r();
equal_to_r <- function(x){
  if (x =='='){
    res <-"=="
  }else{
    res <-x
  }
  return(res)
}
#' 批量处于单个等于为双个等于的比较符
#'
#' @param x 向量
#'
#' @return 返回值
#' @export
#'
#' @examples equal_to_rs();
equal_to_rs <- function(x){
  lapply(x, equal_to_r) %>% unlist
}
#' 将数据集进行过滤
#'
#' @param data 数据框
#' @param fieldName 过滤字段
#' @param comparerSign 比例符号
#' @param value_vec 值设置
#' @param comboLogi_vec 逻辑连接条件
#'
#' @return 返回值
#' @export
#'
#' @examples df_filter();
#' df_filter(iris,fieldName = 'Sepal.Width',comparerSign = '=',value_vec = '3.5',comboLogi_vec = 'and');
df_filter <- function(data=iris,
                      fieldName=c('Species','Sepal.Width'),
                      comparerSign=c('=','>='),
                      value_vec=c("'setosa'","3.0"),
                      comboLogi_vec=c('and','and')){
  data <- substitute(data);
  
  comparerSign <- equal_to_rs(comparerSign);
  comboLogi_vec <-comboLogi_to_rs(comboLogi_vec);
  comboLogi_vec[length(comboLogi_vec)] <-"";
  part1 <-paste(fieldName,comparerSign,value_vec,comboLogi_vec,sep=" ",collapse = " ");
 expr <-paste(
   'res <- filter(',data,',',part1,');',sep = ' ' ) ;
 expr <-R_expr(expr);
 R_exec(expr);
 res <- tbl_as_df(res);
 return(res);
}
