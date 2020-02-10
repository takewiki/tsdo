
#' 在2个数据框中进行自上而下分配运算
#'
#' @param df1 数据1
#' @param df2 数据2
#'
#' @return 取最小行返回数据框
#' @export
#'
#' @examples
#' res <- allocate(data.frame(a=letters),data.frame(b=10:1));
allocate <- function (df1,df2){
  ncount1 <- nrow(df1);
  ncount2 <- nrow(df2);
  #从中获取最小行
  n <- min(ncount1,ncount2)
  print(ncount1)
  print(ncount2)
  print(n)
  if(n >=1){
    res1 <- df1[1:n,,drop=FALSE];
    res2 <- df2[1:n,,drop=FALSE];
    res <-cbind(res1,res2);
  }else{
    res <-NULL
  }



  return(res)
}


