#' 非标准评估捕获输入参数
#'
#' @param x 输入参数
#'
#' @return 返回未经评估的值
#' @export
#'
#' @examples
#' nse_input()
#' nse_input(x+1)
#' # x+1
#' x <- 1
#' nse_input(x)
#' # x
nse_input <- function(x) {
  res <- substitute(x)
  return(res)
  
}

#' NSE获取输入参数的文件形式
#'
#' @param x 参数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nse_input_str()
#' nse_input_str(ggplot2)
#' get the ggplot2
#' nse_input_str(x+y^2)
nse_input_str <- function(x) {
  
  # res <- deparse(nse_input(x))
  # 错误的表达方式
  # always get 'x'
  res <- deparse(substitute(x))
  return(res)
  
}

#' NSE封装过滤相关功能
#'
#' @param x 数据
#' @param condition 条件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nse_subset()
#' nse_subset(iris,Species =='virginica')
#' 总结：直接调用的话，可以不带引号
#' 然后直接使用substitute
#' 因为本身就是用户交互
nse_subset <- function(x,condition) {
  # 形成表达式
  condition_call <- substitute(condition)
  # 评估表达式，指定内容
  r <- eval(condition_call,x,parent.frame())
  #得到结果
  res <- x[r,]
  return(res)
  
}


#' 处理子集用于函数引用
#'
#' @param x 数据集
#' @param condition_q 表达式，自带引号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nse_subset_q()
#' 如果是函数调用的话，必须形成表达式
#' 然后才可以被eval
#' substitute在上一层进行定义
nse_subset_q <- function(x,condition_q) {
  # # 形成表达式
  # condition_call <- substitute(condition)
  # 评估表达式，指定内容
  r <- eval(condition_q,x,parent.frame())
  #得到结果
  res <- x[r,]
  return(res)
  
}




#' NSE封装过滤相关功能
#'
#' @param x 数据
#' @param condition 条件
#'
#' @return 返回值
#'
#' @examples
#' nse_subset()
#' nse_subset2(iris,Species =='virginica')
#' 虽然可以使用，但是把相关的变量从函数带到相应的相应的环境中，有点不友好
nse_subset2 <- function(x,condition) {
  # 形成表达式
  condition_call <- substitute(condition)
  # 评估表达式，指定内容
  env <- list2env(x,parent.frame())
  r <- eval(condition_call,env)
  #得到结果
  res <- x[r,]
  return(res)
  
}





#' 处理取样
#'
#' @param 数据
#'
#' @return 还回值
#' @export
#'
#' @examples
#' nse_scramble()
nse_scramble <- function(x) {
  res <- x[sample(nrow(x)),]
  return(res)
}

#' 测试取子集后随机排序
#'
#' @param x 数据集
#' @param condition  条件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nse_subScramble()
#' 作为顶层函数，定义
#' 本层用于交互，
#' 同时为下一层提供subtitute
#' 也就是说可以顶层才可以使用substitute
#' 其他层全部通过表达式来传递才是函数安全的
nse_subScramble <-function(x,condition){
  #错误的处理方式
  # res <- nse_scramble(nse_subset(x,condition))
  # return(res)
  condition_q <- substitute(condition)
  res <- nse_scramble(nse_subset_q(x,condition_q))
  return(res)
  
  
}


#' 提供一致的数据处理
#'
#' @param x 数据
#' @param env 环境
#'
#' @return 返回值
#' @import pryr
#' @export
#'
#' @examples
#' nse_subs()
#' a <-1
#' b <-2
#' nse_subs(a+b+2)
#' x<-quote(a+b)
#' nse_substitute(x,list(a=1,b=3))

nse_substitute <- function(x,env=parent.frame()) {
  exp <- substitute(substitute(y,env),list(y=x))
  res <- eval(exp)
  return(res)
}



#' 针对图形进行标准化处理
#'
#' @param x 第一个参数
#' @param y 第二个参数
#' @param data 数据庥
#' @param ... 变量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nse_xyplot(mpg,cyl,mtcars)
#' nse_xyplot(mpg,hp,mtcars)
#' nse_xyplot(mpg,disp,mtcars,col="red",aspect="xy")
#' 将原来写入的函数变成比较通用的方式，这是不错的处理方式
nse_xyplot <- function(x,y,data,...) {
  exp <- substitute(xyplot(x~y,data=data,...))
  eval(exp)
  
}


#' 提供对内容的获取
#'
#' @param ... 列表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nse_dots()
nse_dots <- function(...) {
  eval(substitute(alist(...)))
  
}
