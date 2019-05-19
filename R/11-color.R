#' 将RBG模式的颜色转移CMYK模式
#'
#' @param R 红色
#' @param G 绿色
#' @param B 蓝字
#'
#' @return 返回CMYK列表
#' @export
#'
#' @examples RGB2CMYK(255,255,0);
RGB2CMYK <-function(R,G,B){
r1 <- R/255;
g1 <- G/255;
b1 <- B/255;
K <- 1- max(r1,g1,b1);
C <- round((1-r1-K)/(1-K),2)*100;
M <- round((1-g1-K)/(1-K),2)*100;
Y <- round((1-b1-K)/(1-K),2)*100;
K <- round(K,2)*100;
res <- data.frame(C=C,M=M,Y=Y,K=K);
return(res);
}


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


#' 用于将CMYK模式的图片设置成RGB格式
#'
#' @param C 青色CyanW值，0-100之间;
#' @param M 品红色Magenta值，0-100之间；
#' @param Y 黄色Yelloww值，0-100之间；
#' @param K Black or Key Board 值，0-100之间
#'
#' @return 返回列表
#' @export
#'
#' @examples CMYK2RGB(1,0,0,0);
CMYK2RGB <-function (C=0,M=0,Y=0,K=0){
  R <- round(255*(100-C)*(100-K)/10000,0);
  G <- round(255*(100-M)*(100-K)/10000,0);
  B <- round(255*(100-Y)*(100-K)/10000,0);
  res <-data.frame(R=R,G=G,B=B);
  return(res);
}


#' 用于输入一个完成的CYMK资料
#'
#' @param data 输入数据，列表或data.frame,提供C,Y,M,K四个参数
#'
#' @return 返回值
#' @export
#'
#' @examples CYMK(list(C=1:100,Y=1:100,M=1:100,K=1:100));
CMYK <- function(data){
  str_CMYK <- paste(data$C,data$M,data$Y,data$K,sep=",");
  res_RGB <- CMYK2RGB(data$C,data$M,data$Y,data$K);
  hex_u <- RGB2Hex(res_RGB$R,res_RGB$G,res_RGB$B,format = 'upper');
  hex_l <- RGB2Hex(res_RGB$R,res_RGB$G,res_RGB$B,format = 'lower');
  str_RGB <-paste(res_RGB$R,res_RGB$G,res_RGB$B,sep=",");
  res_all<-data.frame(C=data$C,
                      M=data$M,
                      Y=data$Y,
                      K=data$K,
                      CMYK=str_CMYK,
                      R=res_RGB$R,
                      G=res_RGB$G,
                      B=res_RGB$B,
                      RGB=str_RGB,
                      Hex_lower=hex_l,
                      Hex_upper=hex_u,stringsAsFactors = F);
  res_hex <-data.frame(upper=hex_u,lower=hex_l,stringsAsFactors = F);
  res <- list(all=res_all,rgb=res_RGB,hex=res_hex);
  class(res) <-'CMYK';
  return(res);
}

