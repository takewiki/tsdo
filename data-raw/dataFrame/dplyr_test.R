library(dplyr);

summarise(mtcars, avg = mean(mpg));


df_summarise <- function(data=mtcars,name='avssg',fun=mean,field='mpg'){
  
  
   data <- substitute(data);
   fun <- substitute(fun);
   expr <-paste('res <- summarise(',data,",", name,'=', fun,'(',field,'));',sep = "")
   expr <-R_expr(expr);
   R_exec(expr);
   return(res);
}

bb <-df_summarise(data=mtcars,
             fieldName=c('mpg','cyl','disp'),
             funName=c('mean','sum','length'),
             fieldCaption=letters[1:3]
);

class(bb);


?summarise;


data=mtcars,
fieldName=c('mpg','cyl','disp');
funName=c('mean','sum','length');
fieldCaption=letters[1:3];

names(mtcars);


count(mtcars,mpg,disp,cyl);

paste(fieldName,collapse = ",");





df_count(data=mtcars,
             fieldName=c('mpg','cyl','disp')
);


summarise_all(mtcars,sum,length);


bb <-df_summarise_allCols_oneFun(mtcars,sum);
class(bb);
df_summarise_cols_funs();

summarise_at(.tbl = mtcars,fieldName,sum);




df_summarise_cols_oneFun(data=mtcars,
         fieldName=c('mpg','cyl'),length);

df_summarise;


?summarise_if;




mtcars %>%
  group_by(cyl,disp) %>%
  summarise(avg = mean(mpg),sum=sum(disp));


df_groupBy_summarise_cols_funs(mtcars,
                               gp_fieldName = c('mpg','cyl','disp'),
                               gp_fieldCaption = c('gp1','gp2','gp3'),
                               funName = c('sum','sum','sum'),
                               summ_fieldCaption = c('sum1','sum2','sum3'));
