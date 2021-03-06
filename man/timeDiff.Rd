% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-datetime.R
\name{timeDiff}
\alias{timeDiff}
\title{处理两个时间差}
\usage{
timeDiff(x, targetTime, unit = "h", digit = 1)
}
\arguments{
\item{x}{原始时间数据}

\item{targetTime}{目标时间数据}

\item{unit}{差额的时间单位}

\item{digit}{保留小数位数}
}
\value{
返回值
}
\description{
处理两个时间差
}
\examples{
timeDiff('6:00:00','9:00:00')
}
