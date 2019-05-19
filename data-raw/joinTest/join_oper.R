library(dplyr);
?left_join;

library(readxl)
joinData1 <- read_excel("data-raw/joinTest/joinDataExample.xlsx", 
                        sheet = "joinData1")
joinData1 <- tbl_as_df(joinData1);
usethis::use_data(joinData1,internal = F,overwrite = T);

library(readxl)
joinData2 <- read_excel("data-raw/joinTest/joinDataExample.xlsx", 
                        sheet = "joinData2")
joinData2 <- tbl_as_df(joinData2);
usethis::use_data(joinData2,internal = F,overwrite = T);


library(readxl)
joinData3 <- read_excel("data-raw/joinTest/joinDataExample.xlsx", 
                        sheet = "joinData3")
joinData3 <- tbl_as_df(joinData3);
usethis::use_data(joinData3,internal = F,overwrite = T);

library(tsdo);
data("joinData1");
data("joinData2");
left_join(joinData1,joinData2,by=c('fname'='fname','fclass'='fclass'));

library(tsdo);
data("joinData1");
data("joinData2");
filed_by <-c('fname','fclass');
left_join(joinData1,joinData2,by=filed_by);  


library(tsdo);
data("joinData1");
data("joinData2");
filed_by <-c('fname','fclass');
df_leftJoin_byDiffColNames(joinData1,joinData2,filed_by,filed_by);


df_leftJoin_bySameColNames(joinData1,joinData2,filed_by);

df_leftJoin_byDiffColNames(joinData1,joinData2,filed_by,filed_by);



library(tsdo);
data("joinData1");
data("joinData3");
left_by <-c('fname','fclass');
right_by <-c('titileName','titleClass');
df_fullJoin_byDiffColNames(joinData1,joinData3,left_by,right_by);



library(tsdo);
data("joinData1");
data("joinData2");
filed_by <-c('fname','fclass');
df_fullJoin_bySameColNames(joinData1,joinData2,filed_by)


library(dplyr);
?add_row;


add_row(faithful, eruptions = 1, waiting = 1,.before = 4);

  add_row(faithful, eruptions = 1, waiting = 1,.after = 4);


?anti_join();


anti_join(joinData1,joinData2,by='fname');

joinData1;
joinData2;

setdiff(joinData1,joinData2);

joinData3;

anti_join(joinData1,joinData3,by=c('fname'='titileName'));

setdiff(joinData1,joinData3);



df_filter(iris,fieldName = 'Sepal.Width',comparerSign = '=',value_vec = '3.5',comboLogi_vec = 'and');
