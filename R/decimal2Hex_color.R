#' 将十进制的颜色数值转化为16进制，提供大小写处理,内部函数，不再导出
#'
#' @param value 填写数值
#' @param format 返回结果的格式，提供upper及lower等2个选项
#' @return 返回值为字符中
#' @examples  decimal2Hex_color(200,'upper');

decimal2Hex_color <- function (value,format='upper'){
  if (max(value) > 255 ) {
    stop('颜色数值不能大于255!')
    res <-'错误'
  }else{
    index_integer <- value %/% 16 +1;
    index_decimal <- value %% 16 +1;
    if (format == 'lower' ){
      res <- paste(hexBase$L[index_integer],hexBase$L[index_decimal],sep = "");
    }else{
      res <- paste(hexBase$U[index_integer],hexBase$U[index_decimal],sep = "");
    }


  }
return(res);
}

#' 用于将RGB转化为十六制定的表达方式
#'
#' @param R 输入R值
#' @param G 输入G值
#' @param B 输入B值
#' @param format 提供upper或lower2个选择
#' @param prefix 使用前缀，一般为#
#'
#' @return 返回值为一个数据
#' @export
#'
#' @examples RGB2Hex(1:100,3:202,5:204,format = 'lower');
RGB2Hex <- function(R,G,B,format='upper',prefix="#"){
  r1 <-decimal2Hex_color(R,format = format);
  g1 <-decimal2Hex_color(G,format = format);
  b1 <-decimal2Hex_color(B,format = format);
  res<-paste(prefix,r1,g1,b1,sep = "");
  return(res);
}



