#' create a new environment
#'
#' @return return value
#' @export
#'
#' @examples
#' env_new()
env_new <- function() {
  exp <- quote(new.env())
  res <- eval(exp,parent.frame())
  return(res)
  
}

#' create a new environment
#'
#' @return return a new env
#' @export
#'
#' @examples
#' env_create()
env_create <- function() {
  exp <- quote(new.env())
  res <- eval(exp,parent.frame())
  return(res)
  
}


#' get the global environemnt
#'
#' @return return value
#' @export
#'
#' @examples
#' env_global()
env_global <- function(){
  res <- globalenv()
  return(res)
}


#' get the base invironment
#'
#' @return return value
#' @export
#'
#' @examples
#' env_base()
env_base <- function() {
  res <- baseenv()
  return(res)
  
}


#' return the empty enviroment
#'
#' @return return value
#' @export
#'
#' @examples
#' env_empty()
env_empty <- function() {
  res <-emptyenv()
  return(res)
  
}

#' 返回根节点环境
#'
#' @return 返回值
#' @export
#'
#' @examples
#' env_root()
env_root <- function() {
  res <- env_empty()
  return(res)
}


#' 返回当前的环境内容
#'
#' @return 返回值
#' @export
#'
#' @examples
#' env_current()
env_current <- function() {
  exp <- quote(environment())
  res <- eval(exp,parent.frame())
  return(res)
  
}

#' get the search path
#'
#' @return return search path vector
#' @export
#'
#' @examples
#' env_searchPath()
env_searchPath <- function() {
  res <- search()
  return(res)
  
}


#' get the parent env
#'
#' @param env env input
#'
#' @return value
#' @export
#'
#' @examples
#' env_parent()
env_parent <- function(env) {
  exp <- substitute(parent.env(env))
  res <-eval(exp,parent.frame())
  return(res)
  
}



#' show all the object in a env
#'
#' @param env environment
#'
#' @return return object vect
#' @export
#'
#' @examples
#' env_ls()
env_ls <- function(env) {
  res <- ls(env)
  return(res)
  
}

#' 显示一个环境所有变量及值
#'
#' @param env 环境
#'
#' @return 返回值
#' @export
#'
#' @examples
#' env_lsStr()
env_lsStr <- function(env) {
  res <- ls.str(env)
  return(res)
  
}

#' assert envs are the identical
#'
#' @param env1  env1
#' @param env2  env2
#'
#' @return return value
#' @export
#'
#' @examples
#' envs_are_identical()
envs_are_identical <- function(env1,env2) {
  res <-identical(env1,env2)
  return(res)
  
}


