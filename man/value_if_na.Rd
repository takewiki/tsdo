% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-vec_logical.R
\name{value_if_na}
\alias{value_if_na}
\title{如果向量值为NA，则替代为相应的replace_value}
\usage{
value_if_na(x, replace_value)
}
\arguments{
\item{x}{定义一个原生的向量，可以是数值或签字串}

\item{replace_value}{定义一个替代值}
}
\value{
返回一个向量
}
\description{
如果向量值为NA，则替代为相应的replace_value
}
\examples{
 value_if_na(c(1,2,3,NA,NA,4),5);
}
