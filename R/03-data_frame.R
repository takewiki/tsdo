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
      data[,i] <- character()
    } else if(is.list(data[,i])) {
      data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
    }
  }
  
  return(data)
  
}



#' 数据框添加列
#'
#' @param data 数据框
#' @param col_name 列名
#' @param col_value 列值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_addCol()
df_addCol <- function(data=iris,col_name='bbc',col_value='123'){
  res <- data
  res[,col_name] <- col_value
  return(res)
}




#' 将数据框转换成分组列表
#'
#' @param df 数据框
#' @param group_col 分组列名
#' @param value_col 值名
#'
#' @return 返回二维list
#' @export
#'
#' @examples
#' df_as_groupList()
df_as_groupList <- function(df,group_col,value_col) {
  df_split <- split(df,df[,group_col])
  res <-lapply(df_split, function(item){
    value <- item[,value_col]
    r <- tsdo::vect_as_list(value)
    return(r)
  })
  return(res)
}



#' 将数据按标记进行处理
#'
#' @param data 数据
#' @param var_txt 内容
#' @param var_flag 标记
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_combineRows()
df_combineRows <- function(data,var_txt,var_flag){
  data$FCumsum <- cumsum(data[,var_flag])
  data_split <- split(data,data$FCumsum)
  res <- lapply(data_split,function(item){
    txt <-paste(item[,var_txt],collapse = ";")
    flag <- item[1,var_flag]
    FCumsum <- item[1,'FCumsum']
    res <- data.frame(txt,flag,FCumsum,stringsAsFactors = F)
    names(res) <-c(var_txt,var_flag,'FCumFlag')
    return(res)
    
  })
  data_combined <- do.call('rbind',res)
  return(data_combined)
}



#' 针对数据分列处理，按指定列进行处理
#'
#' @param data  数据框
#' @param var_txt  原始内容列
#' @param var_split 分列依据字段
#' @param var_left 左边字段
#' @param left_skip 分列左边忽略字符数
#' @param var_right 右边字段
#' @param right_skip 分列右边忽略字符数
#'
#' @return 返回数据框
#' @export
#'
#' @examples
#' df_splitByCol
df_splitByCol <- function(data,var_txt='FLog',var_split='log_datetime',var_left='author',left_skip=2,var_right='content',right_skip=4){
  data_split <- lapply(1:nrow(data), function(i){
    item <- data[i,]
    
    txt <- item[1,var_txt]
    datetime <- item[1,var_split]
    info <-stringr::str_locate(txt,datetime)
    start_loc <- info[1,1]
    end_loc <- info[1,2]
    item_len <-tsdo::len(txt)
    item[1,var_left] <-tsdo::left(txt,start_loc-left_skip)
    item[1,var_right] <- tsdo::right(txt,item_len-end_loc-right_skip)
    return(item)
  })
  res <- do.call('rbind',data_split)
  return(res)
  
  
}


#' 针对数据框设置标准
#'
#' @param data 数据框
#' @param var_txt 内容字段
#' @param keyword 关键词
#' @param var_flag 标记字段
#'
#' @return 返回一个数据框
#' @export
#'
#' @examples
#' df_setLabel
df_setLabel <- function(data,var_txt='FLog',keyword='捷豹路虎官方旗舰店',var_flag='FIsCsp'){
  
  data[,var_flag]  <- stringr::str_detect(data[,var_txt],keyword)
  return(data)
  
}


#' get the datarightpart
#'
#' @param data data
#' @param var_txt txt
#' @param var_right right
#' @param var_left left
#'
#' @return return
#' @export
#'
#' @examples
#' df_left
df_left <- function(data,var_txt,var_right,var_left) {
     ncount = nrow(data)
     lapply(1:ncount, function(i){
       # 针对第一行进行处理
       nchar_loc <-tsdo::len(data[i,var_txt,drop=TRUE])-tsdo::len(data[i,var_right,drop=TRUE])
      data[i,var_left] <<- tsdo::left(data[i,var_txt,drop=TRUE],nchar_loc)
    })
    return(data)
  
}


#' 提供数据框的右边部分
#'
#' @param data  数据
#' @param var_left 左边已有内容
#' @param var_right 待生成右边内容
#' @param var_txt 文本内容
#'
#' @return 返回值
#' @export
#'
#' @examples
#' df_right()
df_right <- function(data,var_txt,var_left,var_right) {
  ncount = nrow(data)
  lapply(1:ncount, function(i){
    # 针对第一行进行处理
    nchar_loc <-tsdo::len(data[i,var_txt,drop=TRUE])-tsdo::len(data[i,var_left,drop=TRUE])
    data[i,var_right] <<- tsdo::right(data[i,var_txt,drop=TRUE],nchar_loc)
  })
  return(data)
  
  
}





#' 针对数据框进行打款
#'
#' @param data 数据框
#' @param var_text 字符
#' @param var_flag 是否打中
#' @param var_tag_by 判断标记
#' @param dict_contain 包含规则的黑名单
#' @param dict_equal 严格相等的黑名单
#'
#' @return 返回数据框
#' @export
#'
#' @examples
#' df_blackList_tagging()
df_blackList_tagging <-function(data,var_text='Ftxt',dict_contain,dict_equal,var_flag='Fflag',var_tag_by='Ftag_by'){
  
  ncount <-nrow(data)
  if(ncount >0){
    #针对有数据的情况下进行处理
    for (i in 1:ncount) {
      
      
      #针对每一行数据进行处理
      x <-data[i,var_text]
      if(is.na(x)){
        x <- ""
      }
      print(paste0('i',i,x))
      #针对数据进行处理
      #初始化变更
      find <- 0
      flag <- FALSE
      tag_by <-""
      
      #尝试处理
      try({
        #针对每种情况进行处理,顺序也非常重要
        #1网址  优先识别网址
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_http(x)
          if(flag){
            find <- 1
            tag_by <-"网址"
            print('str_contain_http')
          }
        }
        #1手机号-----
        if(find ==0 & flag == FALSE){
          #判断是否手机号
          flag <- str_contain_phone(x)
          if(flag){
            find <- 1
            tag_by <-"电话号码"
            print('str_contain_phone')
          }
        }
        if(find ==0 & flag == FALSE){
          #判断是否手机号
          flag <- str_contain_num(x,digit = 11)
          if(flag){
            find <- 1
            tag_by <-"11位数字"
            print('str_contain_num')
          }
        }
        #2车架号----
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_vin(x)
          if(flag){
            find <- 1
            tag_by <-"车架号"
            print('str_contain_vin')
          }
        }
   
        #4卡片
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_card(x)
          if(flag){
            find <- 1
            tag_by <-"[卡片]"
            print('str_contain_card')
          }
        }
        #5图片.
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_picture(x)
          if(flag){
            find <- 1
            tag_by <-"[图片]"
            print('str_contain_picture')
          }
        }
        #6.表情
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_emotion(x)
          if(flag){
            find <- 1
            tag_by <-"[表情]"
            print('str_contain_emotion')
          }
        }
        #7.语音
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_sound(x)
          if(flag){
            find <- 1
            tag_by <-"[语音]"
            print('str_contain_sound')
          }
        }
        # 8.emoji
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_emoji(x)
          if(flag){
            find <- 1
            tag_by <-"[emoji]"
            print('str_contain_emoji')
          }
        }
        #9.?
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_ask(x)
          if(flag){
            find <- 1
            tag_by <-"问号1"
            print('str_contain_ask')
          }
        }
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_ask2(x)
          if(flag){
            find <- 1
            tag_by <-"问号2"
            print('str_contain_ask')
          }
        }
        if(find ==0 & flag ==FALSE){
          flag <- str_contain_ask3(x)
          if(flag){
            find <- 1
            tag_by <-"问号3"
            print('str_contain_ask')
          }
        }
        #10处理空行数据
        if(find ==0 & flag ==FALSE){
          #NA作为空白行进行处理
          if(is.na(x)){
            x <-""
          }
          flag <- str_contain_blank(x)
          if(flag){
            find <- 1
            tag_by <-"空白行"
            print('str_contain_blank')
          }
        }
        
        #11使用对照表进行处理
        #A处理包含匹配的
        if(find ==0 & flag ==FALSE){
          #针对规则表中的每一个数据处理
          for (rule in dict_contain) {
            #for start
            
            try({
              
              res <-stringr::str_detect(x,rule)
              if(res){
                print(paste0('j',rule))
                flag <- TRUE
                tag_by<- rule
                find <-1
                break;
              }else{
                next;
              }
            })
  
            
            #for end
            
          }
        }
        #针对B业务处理，严格相等
        if(find ==0 & flag ==FALSE){
          #针对规则表中的每一个数据处理
          for (rule in dict_equal) {
            #start for
            try({
              res <- x == rule
              if(res){
                print(paste0('2j',rule))
                flag <- TRUE
                tag_by<- rule
                find <-1
                break;
              }else{
                next;
              }
              
            })
    
            
          #end for
            }
        }
        
        
        # end for 10
        
      })
      
     
      
      data[i,var_flag] <- flag
      data[i,var_tag_by] <- tag_by
      
      
      
      
      
      
      
      
    }#end for row deal
    
    
  }
  
  return(data)
  
  
  
}








