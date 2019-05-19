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


lag(letters);


df_groupBy_summarise_cols_funs(mtcars,
                               gp_fieldName = c('mpg','cyl','disp'),
                               gp_fieldCaption = c('gp1','gp2','gp3'),
                               funName = c('sum','sum','sum'),
                               summ_fieldCaption = c('sum1','sum2','sum3'));


data1 <-data.frame(fname=letters[1:10],fage=21:30,stringsAsFactors=F);
data2 <-slice(data1,5:10);
data2 <-rbind(data2,data.frame(fname=letters[11:20],fage=31:40,stringsAsFactors=F));


setdiff(data1,data2);

setdiff(data2,data1);

intersect(data1,data2);

union(data1,data2);
union_all(data1,data2);

setdiff;

intersect;


iris %>% group_by(Species,Petal.Width) %>%
top_n( 2, Sepal.Width)

iris;


df_groupBy_topN(data = iris,gp_fieldName = 'Species',order_fieldName = 'Sepal.Width',N = 2
                
                  );
