#' 将数据框的行与列进行转化，
#'
#' @param data  数据框，要求第一列必须为文本，后续用于形成列标题
#'
#' @return   返回转化后的数据框
#' @export
#'
#' @examples rowColEx(data);
rowColEx <- function ( data)
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



