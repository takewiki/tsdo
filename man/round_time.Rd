% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05b_time.R
\name{round_time}
\alias{round_time}
\title{针对日期数据进行取舍}
\usage{
round_time(x, unit = c("hour", "min"))
}
\arguments{
\item{x}{原始数据，完整时间类型的字符向量}

\item{unit}{四舍五入的单位}
}
\value{
返回值
}
\description{
针对日期数据进行取舍
}
\examples{
round_time('9:12:05','min');
}
