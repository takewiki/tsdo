setExample1<-data.frame(fname=letters[1:10],fage=21:30,stringsAsFactors=F);
data2 <-slice(data1,5:10);

setExample2 <-rbind(data2,data.frame(fname=letters[11:20],fage=31:40,stringsAsFactors=F));

usethis::use_data(setExample1,internal = F,overwrite = T);
usethis::use_data(setExample2,internal = F,overwrite = T);
