#' 提供文本中的电话及地址信息
#'
#' @param file
#'
#' @return
#' @export
#' @import readxl
#' @import stringr
#'
#' @examples
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







