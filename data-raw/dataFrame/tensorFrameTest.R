library(readxl)
tensorFrameExample <- read_excel("data-raw/tensorFrame/tensorFrameExample_needFill.xlsx")
#View(tensorFrameExample)
tensorFrameExample <- as.data.frame(tensorFrameExample);
#获取所有字段的类型
fieldType <- unlist(lapply(tensorFrameExample, class));
#获取所有的字段名
fieldName <-names(fieldType);
value_var <-'Fvalue';
#判断数值字段
value_idx <-fieldName == value_var;
 
#获取字段名
fieldName_value <- fieldName[value_idx];
fieldName_categories <- fieldName[!value_idx];
#获取字段类型
fieldType_value <-fieldType[fieldName_value];
fieldtype_categories <-fieldType[fieldName_categories];

#判断类别字段类型是否非法
is.categoryType.invalid <- function(fieldtype_categories){
  
 res <- !sum(as.integer(fieldtype_categories == 'character')) == length(fieldtype_categories)
 return(res)
  }
is.categoryType.invalid(fieldName_categories);

#判断字段类型是否非法
is.valueType.invalid <- function(fieldType_value){
  
  res <- !sum(as.integer(fieldType_value == 'numeric')) == length(fieldType_value)
  return(res)
}
is.valueType.invalid(fieldType_value);

#
#' 针对原始数据进行标准化，所有的行使用character,所有的value使用numeric
data_categories <-tensorFrameExample[,fieldName_categories];
data_categories;
#View(data_categories);
data_categories <- lapply(data_categories,as.character);
data_categories <- as.data.frame(data_categories,stringsAsFactors=FALSE);
#View(data_categories);
data_value <- tensorFrameExample[,fieldName_value,drop=FALSE];
data_value <-lapply(data_value,as.numeric);
data_value <-as.data.frame(data_value,stringsAsFactors = FALSE);
tensorFrameExample_normalized <-cbind(data_categories,data_value);
View(tensorFrameExample_normalized);

#获取数据集的相关信息

edgeNames <-data_categories %>% lapply(unique);
edges <- edgeNames %>% lapply(length) %>% unlist();
tensorCapacity <- edges %>% prod();
tensorCapacity;

#判断是否需要插补数据

is.need.fill <-tensorCapacity != dim(tensorFrameExample_normalized)[1]

tensorFrame.fillData

data_categories;
edgeNames;
edges;

ftype_element <- edgeNames[[4]];
ftype_tpl <-rep(ftype_element,each=1)
ftype_repCount <-prod(edges[1:3]);

ftype_data <-rep(ftype_tpl,ftype_repCount);
#ftype_data;
FAccountID_element <- edgeNames[[3]];
FAccountID_tpl <- rep(FAccountID_element,each=prod(edges[4]));
FAccountID_repCount <-prod(edges[1:2]);
FAccountID_data <-rep(FAccountID_tpl,FAccountID_repCount);

FPeriod_element <- edgeNames[[2]];
FPeriod_tpl <-rep(FPeriod_element,each=prod(edges[3:4])); 
FPeriod_repCount <-prod(edges[1]);
FPeriod_data <- rep(FPeriod_tpl,FPeriod_repCount);
FPeriod_data;


FYear_element <- edgeNames[[1]];
FYear_tpl <-rep(FYear_element,each=prod(edges[2:4]));
FYear_repCount <- 1; 
FYear_data <- rep(FYear_tpl,FYear_repCount);
FYear_data;


tensorFrame_fill <- function(edgeNames) {
  #获取边的数据
  edges <- edgeNames %>% lapply(length) %>% unlist ;
  #获取辅助信息
  edge_index <- seq_along(edges);
  edge_len <- length(edges);
  #处理each信息
  each_data <-c(edges[-1],1);
  names(each_data) <- names(edges);
  each_counter <-lapply(edge_index, function(i){
    prod(each_data[i:edge_len])
  })
  each_counter <-unlist(each_counter);
  names(each_counter) <- names(edges);
  #each_counter;
  #处理times处理
  times_data <-c(1,edges[-edge_len]);
  names(times_data) <- names(edges);
  times_counter <-lapply(edge_index, function(i){
    prod(times_data[1:i])
  })
  times_counter <-unlist(times_counter);
  names(times_counter) <-names(edges);
  
  #times_counter;
  
  #处理最终数据
  res<- lapply(edge_index,function(i){
    element <- edgeNames[[i]];
    tpl <-rep(element,each=each_counter[i]);
    data <- rep(tpl,times_counter[i]);
      })
  res <- as.data.frame(res,stringsAsFactors=FALSE);
  names(res) <- names(edges);
  #View(res);
  return(res);
}



tensorFrameFilled <-tensorFrame_fill(edgeNames);

mergeColumns <- function(data_frame){
  do.call('paste',c(data_frame,sep=""));
}


tensorFrameFilled_flag <-mergeColumns(tensorFrameFilled);
tensorFrameFilled_flag;

data_categories_flag <- mergeColumns(data_categories);

tensorFrameFilled[!tensorFrameFilled_flag %in% data_categories_flag,];








