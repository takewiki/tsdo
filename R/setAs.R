#' @include salesOrderHead.R FInterId.R FNumber.R FName.R FDate.R
setAs('salesOrderHead','data.frame',function (from)
  {
   FInterId <- FInterId(from);
   FNumber <- FNumber(from);
   FName <- FName(from);
   FDate <- FDate(from);
   res <- data.frame(FInterId,FNumber,FName,FDate,stringsAsFactors = F);
})
