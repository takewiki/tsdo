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
