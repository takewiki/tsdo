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
