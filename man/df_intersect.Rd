% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-df_setsOpers.R
\name{df_intersect}
\alias{df_intersect}
\title{取两个数据框的交集}
\usage{
df_intersect(data_frame1, data_frame2)
}
\arguments{
\item{data_frame1}{第一个数据框}

\item{data_frame2}{第二个数据框}
}
\value{
返回值
}
\description{
取两个数据框的交集
}
\examples{
library(tsdo);
data('setExample1')
data('setExample2')
df_intersect(setExample1,setExample2);
}
