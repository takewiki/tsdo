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
  phone_part <-str_match(srcdata,"\\d?\\d?\\d?\\d?-?\\d{7}\\d?|1[34578]\\d{9}");
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


#' 提供文本中的电话及地址信息
#'
#' @param file 录入Excel文件名
#'
#' @return 返回值
#' @export
#' @import readxl
#' @import stringr
#'
#' @examples addrPhoneExtrator();
addrPhoneExtractor <- function(file="~/pkgs/tsda/data-raw/src_data/addrPhone_data.xlsx") {
  addrPhone_data <- read_excel(file);
  addrPhone_data <- as.data.frame(addrPhone_data);
  srcdata <- addrPhone_data$addrPhone;
  phone_part <-str_match(srcdata,"\\d?\\d?\\d?\\d?-?\\d{7}\\d?|1[34578]\\d{9}");
  phone_part <- as.character(phone_part);
  len_addr <-len(srcdata)-len(phone_part);
  addr_raw <-lapply(seq_along(len_addr),function(i){
    left(srcdata[i],len_addr[i])
  });
  addr_part <- unlist(addr_raw);
  res <- data.frame(srcdata,addr_part,phone_part,stringsAsFactors = FALSE);
  return(res);
}


