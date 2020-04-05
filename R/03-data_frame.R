#  返回数据值的每列的数据类型 ----
#' 返回数据值的每列的数据类型 
#'
#' @param data_frame 数据框
#'
#' @return 返回值
#' @export
#'
#' @examples df_columnType();
df_columnType <- function(data_frame) {
  if (class(data_frame) != 'data.frame'){
    stop('数据类型必须为data.frame！',call. = FALSE)
  }
  res <- unlist(lapply(data_frame, class));
  return(res);
}
# 获取数据框的列名----
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

# 合并data_frame的列信息，用于文本合并----
#' 合并data_frame的列信息，用于文本合并
#'
#' @param data_frame 数据框
#'
#' @return 返回值
#' @export
#'
#' @examples df_mergeColumn();
df_mergeColumn <- function(data_frame){
  do.call('paste',c(data_frame,sep=""));
}


#' 增加对任意数据框进行排序功能
#'
#' @param srcData 源数据框
#' @param order_by 排序字段，向量
#' @param is_asc 逻辑变更，默认T为升序
#'
#' @return 返回值
#' @export
#'
#' @examples df_orderData()
df_orderData <- function(srcData='data',order_by,is_asc=order_d){
  order_d <- rep(T,length(order_by));
  #order_by <- names(test)[1:4];
  order_by2 <- paste(order_by,collapse = ",");
  #处理排序字段逻辑
  is_asc2 <- !is_asc;
  is_asc2 <- paste(is_asc2,collapse = ',');
  #
  newData <-'res_ordered';
  expr <-paste(newData," <- ",
               srcData,"[with(",srcData,
               ",order(",order_by2,
               ",decreasing = c(",is_asc2,"),method='radix')),]",
               sep = "")
  expr_pared <-parse(text = expr)
  eval(expr = expr_pared)
  return(res_ordered);
  
}

# 数据框选取列----
#' 数据框选取列
#'
#' @param data 数据框
#' @param colSelected 列名 
#'
#' @return 返回值
#' @import dplyr
#' @export
#'
#' @examples
#' df_selectCol();
df_selectCol <- function(data,colSelected) {
  res <-select(data,colSelected);
  return(res);
}

#' 将 df列转化为sqlserver select field字符串
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples df_col2sqlFieldString();
df_col2sqlFieldString <- function (data)
{
  paste0("'",data,"',");
}



#' 将数据框的行与列进行转化，
#'
#' @param data  数据框，要求第一列必须为文本，后续用于形成列标题
#'
#' @return   返回转化后的数据框
#' @export
#'
#' @examples rowColEx(data);
df_rowColEx <- function ( data)
{
  col_name <- names(data);
  
  #str(col_name[-1]);
  #col_name;
  row_name <-data[,1];
  #row_name;
  data_matrix <- as.matrix(data[,-1]);
  data_tmp <- t(data_matrix);
  data_rs <-as.data.frame(data_tmp);
  data_rs$category <- col_name[-1];
  ncol<-dim(data_rs)[2];
  ncol2<- ncol-1;
  data_rs <-data_rs[,c(ncol,1:ncol2)];
  names(data_rs) <- c(col_name[1],row_name);
  data_rs;
}


#' 将具有一行数据的数据框复制为多行
#'
#' @param data 原始数据框
#' @param times 复制行数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_rowRepMulti();
df_rowRepMulti <- function(data,times =6) {
  
  if (nrow(data) != 1){
    stop("data参数对应的数据框要求行数必须为1",call. = FALSE)
  }
  res <- as.data.frame(lapply(data,rep,times=times),stringsAsFactors=FALSE);
  colnames(res) <- colnames(data);
  return(res);
  
}


#' 根据日期及时间数据拆分笔
#'
#' @param data  数据
#' @param startDateName  开始日期字段 
#' @param endDateName  结束日期字段
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_rowRepMultiByDate();
df_rowRepMultiByDate <- function(data,
                                  startDateName='FStartDate',
                                  endDateName = 'FEndDate') {
  startDate <- data[1,startDateName,drop=TRUE];
  startDate <- as.date(startDate);
  endDate <- data[1,endDateName,drop=TRUE];
  endDate <- as.date(endDate);
  date_count <- date_diff(startDate,endDate,is.hr = TRUE);
  data_cols <- names(data);
  date_cols <- c(startDateName,endDateName);
  ot_cols <- data_cols[! data_cols %in% date_cols];
  data_ot <- data[ , ot_cols];
  res <-df_rowRepMulti(data = data_ot,date_count);
  data_date <- date_minus(startDate,endDate);
  res[ ,startDateName] <-data_date;
  res[ ,endDateName] <- data_date;
  res <- res[ ,data_cols];
  return(res);
  
}
#' 将数据框中的所有列作为文本处理
#'
#' @param df 处理框
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_as_character();
df_as_character <- function(df) {
  res <-lapply(df, as.character);
  res <- as.data.frame(res,stringsAsFactors=F);
  return(res);

  
}


#' 将数据框的2列N行转化为N列2行
#'
#' @param data 原始数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#'df_row2Col(); 
df_row2Col <-function(data){
  data2 <- t(data);
  col_names <-data2[1,];
  data2 <-data2[-1,,drop=FALSE];
  colnames(data2) <-col_names
  rownames(data2) <-NULL;
  return(data2)
}


#' 设置列名
#'
#' @param data 数据框 
#' @param col_names 列名
#' @param caption_names  列标称名
#'
#' @return 返回数据框
#' @export
#'
#' @examples
#' df_setColCaption()
df_setColCaption <- function(data,col_names,caption_names){
  res <- data[,col_names]
  names(res) <- caption_names
  return(res)
  
}



#' 将列如果是列表转移成字符串
#'
#' @param data 数据框
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_setList_char()
df_setList_char <- function(data){
  
  for(i in 1:ncol(data)) {
    if(nrow(data) == 0) {
      thedata[,i] <- character()
    } else if(is.list(data[,i])) {
      data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
    }
  }
  
  return(data)
  
}




