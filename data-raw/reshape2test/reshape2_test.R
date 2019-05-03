library(reshape2);
library(tsdo)
#加载数据集
data('airquality');
# 查看列名称
names(airquality);
#列名转化为全小写
names(airquality) <- tolower(names(airquality));
#查看数据
class(airquality);
df_columnType(airquality);
#查看数据
head(airquality);

#查看melt的默认函数
aql <- melt(airquality);
# No id variables; using all as measure variables
aql;
class(aql);
df_columnType(aql);
#所有的变量名称进入variable; 
#所有的值为value

# 查看其他数据
tail(aql);

#针对id进行定义，应该是在行标签的记录

aq_month_day <- melt(airquality,id.vars = c('month','day'));

df_columnType(aq_month_day);

unique(aq_month_day$variable);
class(aq_month_day);
View(aq_month_day);

# 如果只指定id,其中列的数据将分variable,value按行进行叠放
# 这样会增加相应的长度
# 如上的信息variable,value可以进行修改，按业务进行命名

aq_month_day_cust_name <- melt(data = airquality,id.vars = c('month','day'),
                               variable.name = 'climate_var',
                               value.name ='Fvalue' 
                                )
#此数据可用于tensorBag测试-----
head(aq_month_day_cust_name,10);
class(aq_month_day_cust_name);



#以下用于数据分析
aql <- melt(airquality,id.vars = c('month','day'));

head(aql);
#用于数据的转换，还是非常容易的
aqw <- dcast(aql,month+day~variable);
aqw;
df_columnType(aqw);
#的确数据类型已经变化

# group by 数据的丢失才会造成聚合。
# id_var
# variable
# value

aqw_length <- dcast(aql,month~variable);
aqw_length;

#缺少id变更才会形成聚合。

aqw_sum <- dcast(aql,month~variable,sum,na.rm=TRUE);
aqw_sum;

 


