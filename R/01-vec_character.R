# 获取字符赂量中每个元素的长度，而不是向量的长度----
#' 获取字符赂量中每个元素的长度，而不是向量的长度
#'
#' @param x  字符型数据
#'
#' @return   数值型向量
#' @export
#' @import stringr
#' @examples len(letters);
len <- function (x)
{
  if (class(x) !='character')
  {
    stop("'x'参数必须是字符型向量!")
  }else{
    str_length(x)
  }
}

# 从左边截取n个字符----
#' 从左边截取n个字符
#'
#' @param x 字符串向量
#' @param num_char  字符个数
#'
#' @return 返回字符向量
#' @export
#' @import stringr
#' @examples left(letters,1);
left <- function (x,num_char=1){
  nmax <- max(len(x));
  if (is.na(num_char)){
    ""
  }else if(is.na(nmax)){
    ""
    
  }else if (num_char >= nmax)
  {
    x
  }else{
    str_sub(x, 1, num_char);
  }
  
}
# 从右边截取指定位数的字符串-----
#' 从右边截取指定位数的字符串
#'
#' @param x 签字文本
#' @param num_char  签字位数
#'
#' @return 返回字符串
#' @export
#' @import stringr
#' @examples right(letters,3);
right <- function (x,num_char){
  nstart <- len(x)-num_char+1;
  ncount <-length(x);
  res <- list(ncount);
  for (i in 1:ncount){
    res[[i]]=str_sub(x[i],nstart[i],-1L);
  }
  res <-unlist(res);
  res;
  
}

# 从字符中间进行测试-----
#' 从字符中间进行测试
#'
#' @param x 字符串
#' @param start 开始位置
#' @param num_char 位数
#'
#' @return 返回字符串
#' @export
#' @import stringr
#' @examples mid('sdfdsfdsf',2,4);
mid  <- function (x,startIndex,num_char){
  endIndex <- startIndex+num_char-1;
  str_sub(x,startIndex,endIndex);
}

# 将字符串进行拆分开-----
#' 将字符串进行拆分开
#'
#' @param x 字符串
#' @param pattern 可以使用字符串或正则表达式[]
#'
#' @return 返回一个列表
#' @export
#' @import stringr
#' @examples splitStr('afsdsdf,bbbfsdfds,sdfds',',');
splitStr <- function (x,pattern){
  str_split(x,pattern);
}

# 自动过滤符合条件的字符串,也称为字符串的子集或截取-----
#' 自动过滤符合条件的字符串,也称为字符串的子集或截取
#'
#' @param x 字符向量
#' @param pattern 可以使用文本或表达式
#'
#' @return 返回符合条件的结果
#' @export
#' @import stringr
#' @examples subsetStr(letters,'a');
subsetStr <- function (x,pattern)
{
  str_subset(x,pattern);
}

# 文件中是否包含字段检查----
#' 文件中是否包含字段检查
#'
#' @param x 字符向量
#' @param pattern 检查模式
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' str_contain(letters,'a');
str_contain <- function(x,pattern){
  str_detect(x,pattern)
}

# 复制上一行数据-----
#' 复制上一行数据-----
#'
#' @param x 字符型向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_copyPrevRow();
str_copyPrevRow <- function(x){
  ncount <- length(x);
  i <-1;
  while(i < ncount){
    if ( x[i+1] == ""){
      x[i+1] <- x[i];
      
    } else {
      x[i+1] <- x[i+1];
      
    }
    i <- i+1;
  }
  return(x)
}

#' 将一列数据分解为多列数据,返回列表
#'
#' @param x  将要处理的处理
#' @param sep 分隔符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_splitCols();
str_splitCols <- function(x,sep=",") {
  
  
  res_split <-splitStr(x,sep);
  #记录行数
  list_count <- length(res_split);
  #拆分列数
  col_count <-max(unlist(lapply(res_split, length)));
  #初始化结果
  res <- list();
  #使用2个循环进行处理
  for (j in 1:col_count){
    #按列
    res[[j]] <- character(list_count);
    for (i in 1:list_count){
      #按行
      res[[j]][i] <- res_split[[i]][j];
    }
  }
  return(res);
}

#' 将字符串分解为多列，使用英文状态下的左括号，允许前后添加内容；
#'
#' @param x  字符向量
#' @param prefix 左括号前缀
#' @param suffix 左括号后缀
#'
#' @return 返回结果列表
#' @export
#'
#' @examples
#'str_splitByLeftBrace(); 
str_splitByLeftBrace <- function(x,prefix="",suffix="") {
  pattern <-paste(prefix,"\\(",suffix,sep="");
  res <- str_splitCols(x,pattern);
  return(res);
}

#' 将字符串进行拆分为多列，使用右括号
#'
#' @param x 字符向量
#' @param prefix 右括号前缀
#' @param suffix 右括号后缀
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_splitByRighttBrace();
str_splitByRighttBrace <- function(x,prefix="",suffix="") {
  pattern <-paste(prefix,"\\)",suffix,sep="");
  res <- str_splitCols(x,pattern);
  return(res);
}
#' 固定字符的对比
#'
#' @param x  字符串向量
#' @param key  对比字符
#'
#' @return 返回一个向量
#' @export
#'
#' @examples
#' str_equal();
str_equal <- function(x,key){
  res <- x == key ;
  return(res);
}
#' 将字符串用于多个关键词的对比
#'
#' @param x 字符向量
#' @param keys 对比关键词
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_equals();
str_equals <- function(x,keys) {
  ncount <- length(keys);
  res <- list();
  for( i in 1:ncount){
    res[[i]] <-str_equal(x,keys[i]);
  }
  
  p1 <-paste(' res[[',1:ncount,']] ',sep = "",collapse = "|");
  p2 <- paste('res <- ',p1,sep="");
  expr <- R_expr(p2);
  R_exec(expr);
  return(res);
}

#' 判断字符串中是否包含特定的多个字符
#'
#' @param x   字符串向量
#' @param keys 多个关键词
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' str_contains(); 
str_contains <- function(x,keys) {
  
  index <- seq_along(keys);
  res <- list();
  for ( i in index)
  {
    res[[i]] <- str_contain(x,keys[i])
  }
  res<-or_multiple(res);
  return(res);
  
}

# 用于字符串的替代------
#' 用于字符串的替代
#'
#' @param x 字符向量
#' @param pattern 字符或正则表达式
#' @param newStr 新的替代字符串
#'
#' @return 返回值
#' @export
#' @import stringr
#' @examples replaceStr('abcd','c','f');
replaceStr <- function (x,pattern,newStr)
{
  str_replace(x,pattern,newStr);
}

# 获取电话号码-----
#' 获取电话号码
#'
#' @param x 包括电话号码的文本向量
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples getPhoneNumber();
getPhoneNumber <- function(x) {
  srcdata <- x;
  #添加分机号的识别
  phone_part <-str_match(srcdata,"\\d?\\d?\\d?\\d?-?\\d{7}\\d?-?\\d?\\d?\\d?\\d?|1[34578]\\d{9}");
  res <- as.character(phone_part);
  
  return(res);
}

# 设置null值为相应的替代值------
#' 设置文本为null值为相应的替代值
#'
#' @param x 原始向量
#' @param value 替代的数据
#'
#' @return 返回值
#' @export
#'
#' @examples ifnull();
ifnull <- function(x,value=""){
  x[x == 'null'] <- value
}

#'  用于存储数字的大写形式
#'
#' @return 返回值
#' @export
#'
#' @examples numUpperCase();
numUpperCase <- function() {
  res <- c(`1`='壹',
           `2`='贰',
           `3`='叁',
           `4`='肆',
           `5`='伍',
           `6`='陆',
           `7`='柒',
           `8`='捌',
           `9`='玖',
           `10`='拾',
           `100`='佰',
           `1000`='仟',
           `10000`='万',
           `100000000`='亿',
           yuan='元',
           jiao='角',
           fen='分',
           ling='零'
  );
  return(res);
  
}





#' 将结果进行处理显示
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' segList();
segList <- function(data) {
  
  #data <- list(letters,LETTERS);
  
  res <- lapply(data,function(row){
    paste(row,collapse = " ");
  })
  
  res <- as.character(res);
  res <- paste(res,collapse = "\n");
  
}

#' 将结合结果由列表变成向量
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' segList2Vec();
segList2Vec <- function(data) {
  
  #data <- list(letters,LETTERS);
  
  res <- lapply(data,function(row){
    paste(row,collapse = " ");
  })
  
  res <- as.character(res);
  #res <- paste(res,collapse = "\n");
  return(res)
  
}

#' 将编码器的多行文本转化为向量
#'
#' @param text 原始文本
#'
#' @return 返回值
#' @export
#'
#' @examples
#' editor_char();
editor_char <- function(text){
  res <-strsplit(text,"\n");
  res <- res[[1]]
  return(res);
}


#' 处理将向量的每个元素装入一个列表
#'
#' @param x 原始向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vect_as_list(letters);
vect_as_list <- function(x){
  ncount <- length(x);
  res <- list_init(ncount);
  for (i in 1:ncount){
    res[[i]] <-x[i]
  }
  return(res);
}


#' 将列表转化为字段串
#'
#' @param x 列表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' list_as_vect();
list_as_vect <- function(x) {
  res <-unlist(x);
  res <- paste(res,collapse = "\n")
  return(res);
}


#' 将向量转化为长字符串
#'
#' @param x 向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vect_as_long_string();
vect_as_long_string <- function(x){
  res <- paste(x,sep="",collapse = "")
  return(res);
}

#' 处理字符串连接问题
#'
#' @param x 字符赂量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vect_as_dbl_equal();
vect_as_dbl_equal <- function(x){
  res <- paste(x,sep="",collapse = "||")
  return(res);
}


#' 合并多行数据
#'
#' @param data 数据框
#' @param id_var int类型的内码
#' @param value_var 变量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' combine_rows_into_longColumn();
combine_rows_into_longColumn <- function(data,id_var,value_var) {
  col_names <-c(id_var,value_var);
  bb <- unique(data[,col_names])
  FId <- as.character(bb[,id_var]);
  FTxt <- bb[,value_var];
  res <-split(FTxt,FId);
  res <- lapply(res,tsdo::vect_as_long_string);
  res <-unlist(res);
  FId <- as.integer(names(res));
  FTxt <-res;
  data <- data.frame(FId,FTxt,stringsAsFactors = F);
  names(data) <- col_names;
  return(data);
  
  
}
#' 处理向量不包含的问题
#'
#' @param all 全部
#' @param part 部分
#'
#' @return 返回值
#' @export
#'
#' @examples
#' vector_not_in();
vector_not_in<- function(all,part){
  con <- ! all %in% part;
  res <- all[con]
  return(res)
}




#处理订单号


#' 针对空值进行替代
#'
#' @param test_value  测试值
#' @param replace_value 替代值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' na_value();
na_value <- function(test_value,replace_value){
  if(is.na(test_value)){
    res <- replace_value
  }else{
    res <- test_value
  }
  return(res)
}

#' 针对数据进行处理
#'
#' @param test_values 测试值 
#' @param replace_values 替代值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' na_values();
na_values <-function(test_values,replace_values){
  idx <- seq_along(test_values);
  res <- list_init(length(idx));
  for (i in idx){
    res[[i]] <- na_value(test_values[i],replace_values[i])
  }
  res<-unlist(res);
  return(res);
              
}


#' 设置分页处理界面
#'
#' @param volume 总页数
#' @param each_page 每页数量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' paging_setting();
paging_setting <- function(volume =20000L,each_page=7000L){
  #实现向上取整
  ncount <- as.integer(round(volume/each_page+0.4999,0));
  ncount2 <- ncount -1L;
  FStart <- integer(ncount);
  FEnd <- integer(ncount);
  for (i in 1:ncount2){
    FStart[i] <-1L+ each_page*(i-1);
    FEnd[i] <- each_page*i;
  }
   FStart[ncount] <-1L+ each_page*(ncount2);
   FEnd[ncount] <-volume;
   res <- data.frame(FStart,FEnd,stringsAsFactors = F)
   return(res);
  
}


#' 处理连接问题
#'
#' @param ... 列表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sql_paste()
sql_paste <- function(...){
  paste(...,sep = "")
}


#' 处理两个字符向量的笛卡尔积展开
#'
#' @param x 第一个向量
#' @param y 第二个向量
#' @param sep 分隔符
#'
#' @return 返回向量
#' @export
#'
#' @examples
#' text_prod(letters,LETTERS);
text_prod <- function(x,y,sep="#") {
  x_count <- length(x);
  y_count <- length(y);
 res <- character(x_count*y_count);
 k <-1;
 for (i in 1:x_count) {
   for (j in 1:y_count) {
     res[k] <- paste(x[i],y[j],sep=sep);
     k = k+1;
   }
 }
 return(res)
  
}


#' 将数据框按行按行进行扩展
#'
#' @param data 数据
#' @param var_rows 行上数据
#' @param var_cols 列上数据
#' @param var_values 显示数据
#'
#' @return 返回值 
#' @export
#'
#' @examples
#' text_dcast();
text_dcast <- function(data,
                       var_rows=c('FNumber','FName'),
                       var_cols='FType',
                       var_values=c('FSPID','FSPName')
                             ) {
  col_name <- names(data);
  sel_name <- c(var_rows,var_values,var_cols);
  bbc <- data[,sel_name];
  type_unique <-unique(as.character(bbc[ ,var_cols,drop=TRUE]));
  name_add <- text_prod(type_unique,var_values);
  type_count <- length(type_unique);
  value_count <- length(var_values);
  col_index_new <- 1:(type_count*value_count)+ncol(bbc);
  bbc[,col_index_new] <-"";
  name_all2 <- c(col_name,name_add);
  names(bbc) <-name_all2;
  #处理数据------
  type_col <-as.character(bbc[ ,var_cols,drop=TRUE]);
  bbc_split <- split(bbc,type_col);
  
  bbc_combine <- lapply(bbc_split, function(data){
    ftype_find <- as.character(unique(data[,var_cols,drop=TRUE]));
    name_find <- paste(ftype_find,var_values,sep="#");
    name_find_count <- length(name_find);
    for ( i in 1:name_find_count) {
      data[,name_find[i]] <- data[var_values[i]];
      
    }
    return(data)
    
    
  })
  bbc_res <- do.call('rbind',bbc_combine);
  name_res <- c(var_rows,name_add);
  res <-bbc_res[ ,name_res];
  return(res)
}

#' 自动补全功能
#'
#' @param x 字符串
#' @param suffix 后缀
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_suffix_complete()
str_suffix_complete <- function(x,suffix="/"){
  n = length(suffix)
  if(tsdo::right(x,n) != suffix){
    res <- paste(x,suffix,sep="")
  }else{
    res <-x
  }
  return(res)
  
}


#' 自动添加相关词语
#'
#' @param x  原来语句
#' @param word 现有语句
#' @param blank 是否添加空格
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_add
str_add <- function(x,word="/",blank=FALSE) {
  if(str_detect(x,word)){
    res <- x
  }else{
    if (blank){
      res <- paste0(word," ",x)
    }else{
      res <- paste0(word,x) 
    }
   
  }
  return(res)
  
}


#' 针对数据进行数据
#'
#' @param x 原来值
#' @param value 替代值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' na_replace()
na_replace <- function(x,value) {
  res <- x
  res[is.na(res)] <- value
  return(res)
  
}


#' 删除包含指定行
#'
#' @param x 向量
#' @param pattern 模式 
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_delRows
str_delRows <- function(x,pattern){
  res <- x[!str_detect(x,pattern)]
  return(res)
  
}

#' 检验字段是否包含完整的日期时间
#'
#' @param x 字段
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_HasDateTime()
str_HasDateTime <- function(x){
  res <- str_detect(x,'[1-9]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])\\s+(20|21|22|23|[0-1]\\d):[0-5]\\d:[0-5]\\d')

   return(res)
  
}

#' 获取字段日期时间部分
#'
#' @param x 向量
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' str_extractDatetime()
str_extractDateTime <- function(x) {
  res <- str_extract(x,'[1-9]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])\\s+(20|21|22|23|[0-1]\\d):[0-5]\\d:[0-5]\\d')
  
  return(res)
  
}


#' 获取字段中的日期部分
#'
#' @param x 向量
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' str_extractDate()
str_extractDate <- function(x) {
  res <- str_extract(x,'[1-9]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])')
  
  return(res)
  
}

#' 获取字段的时间部分
#'
#' @param x 向量
#'
#' @return 返回值
#' @import stringr
#' @export
#'
#' @examples
#' str_extractTime()
str_extractTime<- function(x) {
  res <- str_extract(x,'(20|21|22|23|[0-1]\\d):[0-5]\\d:[0-5]\\d')
  
  return(res)
  
}




#' 字符串包含内容
#'
#' @param x 字段
#' @param digit 数字位数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_num() 
str_contain_num <-  function(x,digit=11) {
  expr <- paste0("\\d{",digit,"}")
  res <-stringr::str_detect(x,expr)
  return(res)
}
  


#' 判断文件中是否包含电话号码
#'
#' @param x 向量
#'
#' @return 反回值
#' @export
#'
#' @examples
#' str_contain_phone()
str_contain_phone <-function(x){
  res <- stringr::str_detect(x,"\\d?\\d?\\d?\\d?-?\\d{7}\\d?-?\\d?\\d?\\d?\\d?|1[34578]\\d{9}")
  return(res)
}

#' 用于识别车架号信息vin
#'
#' @param x 信息
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_vin()
str_contain_vin <- function(x){
  #强制转化为大写字母
  x = toupper(x)
  res <- stringr::str_detect(x,"[A-HJ-NPR-Z\\d]{17}")
  return(res)
}


#' 判断是否存在网址
#'
#' @param x 变更 
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_http()
str_contain_http <-function(x){
  #强制转换
  x <- toupper(x)
  res <- stringr::str_detect(x,"HTTP")
  return(res)
  
}

#' 针对卡片进行处理
#'
#' @param x 变量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_card()
str_contain_card <-function(x){
  res <- stringr::str_detect(x,"\\[卡片\\]")
  return(res)
  
  
}


#' 是否包含图片
#'
#' @param x 变量 
#'
#' @return
#' @export
#'
#' @examples
#' str_contain_picture()
str_contain_picture <-function(x){
  res <- stringr::str_detect(x,"\\[图片\\]")
  return(res)
  
  
}



#' 针对表情字段进行判断 
#'
#' @param x 变量
#'
#' @return
#' @export
#'
#' @examples
#' str_contain_emotion()
str_contain_emotion <-function(x){
  res <- stringr::str_detect(x,"\\[表情\\]")
  return(res)
  
  
}


#' 是否包含表情字段
#'
#' @param x 变量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_emoji()
str_contain_emoji <-function(x){
  res <- stringr::str_detect(x,"\\[emoji\\]")
  return(res)
  
  
}

#' 针对语音进行处理
#'
#' @param x 变量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_sound()
str_contain_sound <-function(x){
  res <- stringr::str_detect(x,"\\[语音\\]")
  return(res)
  
  
}




#' 针对问进行处理
#'
#' @param x 变更
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_ask()
str_contain_ask <- function(x){
  #将中文转英文问号
  x <- stringr::str_replace_all(x,"？","?")
  res <- stringr::str_detect(x,"\\?")
  if(res){
    if (len(x) ==1){
      res <- TRUE
      
    }else{
      
      res <- FALSE
      
    }
  }
  

  return(res)
}


#' 文件包含2个问号
#'
#' @param x 内容
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_ask2()
str_contain_ask2 <- function(x){
  #将中文转英文问号
  x <- stringr::str_replace_all(x,"？","?")
  res <- stringr::str_detect(x,"\\?")
  if(res){
    if (len(x) ==2){
      res <- TRUE
      
    }else{
      
      res <- FALSE
      
    }
  }
  
  
  return(res)
}

#' 文件包含3个问号
#'
#' @param x 内容
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_ask3()
str_contain_ask3 <- function(x){
  #将中文转英文问号
  x <- stringr::str_replace_all(x,"？","?")
  res <- stringr::str_detect(x,"\\?")
  if(res){
    if (len(x) ==3){
      res <- TRUE
      
    }else{
      
      res <- FALSE
      
    }
  }
  
  
  return(res)
}


#' 检验空行
#'
#' @param x 向量 
#'
#' @return 返回值
#' @export
#'
#' @examples
#' str_contain_blank()
str_contain_blank <- function(x) {
  x <- as.character(x)
  nlen <-len(x)
  if(nlen == 0 ){
    res <-TRUE
  }else{
    res <- FALSE
  }
  return(res)
}

