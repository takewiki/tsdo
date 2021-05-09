#' 如果数值小于等于某个目标值进行替代为新值
#'
#' @param x 原始数值向量
#' @param targetValue 目标值
#' @param replaceValue 替代值默认为0
#'
#' @return 返回值
#' @export
#'
#' @examples
#' replaceValue_lessThan_targetv();
replaceValue_lessThan_target <- function(x,targetValue=15,replaceValue=0) {
  x[x<= targetValue] <-replaceValue;
  return(x);
  
  
}


#' 将数字按会计格式进行处理
#'
#' @param x 数据
#' @param divided_value 如果是万元版则是10000 
#' @param round_digit 保留小数位数一般为2位小数
#' @param seperator  千分位或万分位
#' @param seperator_digit 千分位或万分位
#' @param currency_symbol 币货符号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' num_to_string_AcctFormat()
#' num_to_string_AcctFormat(1234)
#' num_to_string_AcctFormat(1234.12)
#' num_to_string_AcctFormat(1234.123,currency_symbol = '$')
#' num_to_string_AcctFormat(123123123.123)
#' num_to_string_AcctFormat(123123123.123,divided_value = 10000)
#' num_to_string_AcctFormat(c(123123123.123,123321321.13),divided_value = 10000)
#' num_to_string_AcctFormat(c(123123123.123,123321321.13),divided_value = 10000,currency_symbol = '$')
num_to_string_AcctFormat <- function(x,
                                     divided_value=1.0,
                                     round_digit=2,
                                     seperator=",",
                                     seperator_digit=3,
                                     currency_symbol=""
                                     ) {
  options(scipen = 30,digits = 15)
  # 针对数据进行四舍五入的处理
  x = round(x/divided_value,round_digit)
  #print(x/divided_value)
  #print(x)
  #针对数据进行格式化处理
  res <- prettyNum(x,big.mark = seperator,big.interval = seperator_digit)
  #print(res)
  #添加货币符号
  res <- paste0(currency_symbol,res)
  #返回结果
  return(res)
}


#' 将会计格式金额格式化为数值 
#'
#' @param x 会计格式的数值
#' @param seperator 千分位
#' @param currency_symbol 货币符号
#'
#' @return 返回数值
#' @export
#'
#' @examples
#' string_acctFormat_to_num()
#' string_acctFormat_to_num('$123,123,123.12',currency_symbol = '$')
#' string_acctFormat_to_num('￥123,123,123.12',currency_symbol = '￥')
#' 
string_acctFormat_to_num <- function(x,seperator=",",currency_symbol="") {
 #不需要进行判断,对数据进行数据
  #针对货币符号进行处理
  if (currency_symbol == '$')
  {
    currency_symbol <- paste0("\\",currency_symbol)
  }
  x = gsub(currency_symbol,"",x)
  #针对千分位进行处理
  x = gsub(seperator,"",x)
  #格式化为数值
  res <- as.numeric(x)
  #返回值
  return(res)
}

