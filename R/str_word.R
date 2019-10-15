
#' 处理替代数据
#'
#' @param x  原始数据
#' @param keyWord 关键词
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' vector_col_del()
vector_col_del <- function(x,keyWord){
  res <- str_replace_all(x,keyWord,"");
  return(res)
}


#' 删除相应的词逻辑
#'
#' @param x 原始词
#' @param keyWords  关键词 
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vector_col_dels();
#' vector_col_dels(letters,letters[1:10]);
vector_col_dels <- function(x,keyWords){
  for (keyWord in keyWords){
    x <- vector_col_del(x,keyWord);
  }
  
  return(x)
}

#' 删除左边的空格
#'
#' @param x 向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vector_del_leftBrace()
vector_del_leftBrace <- function(x){
  
  x <-vector_col_dels(x,c("\\["))
  return(x)
}

#' 删除右边空格
#'
#' @param x 向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vector_del_rightBrace();
vector_del_rightBrace <-function(x){
  x <-vector_col_dels(x,c("\\]"))
  return(x)
}
#' 删除空格
#'
#' @param x 向量 
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vector_del_space()
vector_del_space <-function(x){
  x <-vector_col_dels(x,c(" "))
  return(x)
}

#' 针对数据处理删词处理
#'
#' @param data 数据框
#' @param value_var 指定变量
#' @param keyWords 关键词
#'
#' @return 返回值
#' @export
#'
#' @examples
#' word_col_del();
#' library(tsda);
#' mydata <- nsim_read('item_question');
#' View(mydata);
#' mydata2 <- word_col_del(mydata,'FQuestion',c('[图片]','[卡片]'));
#' View(mydata2);
word_col_del <- function(data,value_var,keyWords){
  
  mydata <- data[ ,value_var];
  mydata <- vector_del_leftBrace(mydata);
  mydata <- vector_del_rightBrace(mydata);
  mydata <- vector_del_space(mydata);
  keyWords <-vector_del_leftBrace(keyWords);
  keyWords <-vector_del_rightBrace(keyWords);
  keyWords <- vector_del_space(keyWords);
  mynames <- names(data);
  mydata <-vector_col_dels(mydata,keyWords);
  data[ ,value_var] <- mydata;
  data <- data[,mynames];
  return(data)
}


