
#' 用于输入一个完成的CYMK资料
#'
#' @param data 输入数据，列表或data.frame,提供C,Y,M,K四个参数
#'
#' @return 返回值
#' @include decimal2Hex_color.R
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



