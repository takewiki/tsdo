FInterId <-1:10;
FName <- paste('sample',1:10,sep="");
FNumber <-paste('N00000',1:10,sep="");
FDate <- paste('2018-01-',11:20,sep="");
salesOrderHead_sampleData <-data.frame(FInterId,FName,FNumber,FDate,stringsAsFactors = F);
devtools::use_data(salesOrderHead_sampleData,overwrite = T);
