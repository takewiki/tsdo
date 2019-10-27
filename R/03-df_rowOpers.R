
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

#row_delete

#' 按行删除
#'
#' @param data 数据
#' @param id_var id
#' @param keys 删除关键键
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' row_del_byKeys_is()
row_del_byKeys_is <- function(data,id_var,keys) {
  x <-data[ ,id_var];
  con <- !str_equals(x,keys);
  res <- data[con,];
  #res <- log_delAutId(res);
  return(res);
  
}

#' 根据行进行删除
#'
#' @param data 数据框 
#' @param id_var id
#' @param keys 删除关键词
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' row_del_byKeys_like();
row_del_byKeys_like <- function(data,id_var,keys) {
  x <-data[ ,id_var];
  con <- !str_contains(x,keys);
  res <- data[con,];
  #res <- log_delAutId(res);
  return(res);
  
}



#' 按行删除按数量
#'
#' @param data 数据
#' @param id_var ID
#' @param num_digits 位数 
#'
#' @return 返回值
#' @export
#'
#' @examples
#' row_del_byNum();
row_del_byNum <-function(data,id_var,num_digits){
  x <-data[ ,id_var];
  key <- paste("\\d{",num_digits,"}",sep="")
  con <- is.na(as.character(str_match(x,key)));
  res <- data[con,];
  #res <- log_delAutId(res);
  return(res);
}


#' 删除字母对
#'
#' @param data 数据 
#' @param id_var 内码
#' @param num_digits 数据位数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' row_del_byNumChars()
row_del_byNumChars <-function(data,id_var,num_digits){
  x <-data[ ,id_var];
  key <- paste("[A-Za-z0-9]{",num_digits,"}",sep="")
  con <- is.na(as.character(str_match(x,key)));
  res <- data[con,];
  #res <- log_delAutId(res);
  return(res);
}



#' 针对任意的数据框进行分页处理
#'
#' @param data 数据
#' @param each_page 每页数量 
#' @param skip_row  跳过行数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_paging()
df_paging <- function(data=iris,each_page=7000L,skip_row =0L){
  body_count <- nrow(data) -skip_row;
  startIndex <- 1L+skip_row;
  body_data <-data[startIndex:nrow(data),];
  pages <-paging_setting(body_count,each_page);
  page_count <- nrow(pages);
  res <- list_init(page_count);
  for ( i in 1:page_count){
    body2 <-body_data[pages$FStart[i]:pages$FEnd[i],]
    if(skip_row == 0L){
      res[[i]] <- body2
    }else{
      heading <- data[1:skip_row,];
    
      res[[i]] <- rbind(heading,body2);
    }
  }
  return(res)
  
  
}
