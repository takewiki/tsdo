% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/11-color.R
\name{decimal2Hex_color}
\alias{decimal2Hex_color}
\title{将十进制的颜色数值转化为16进制，提供大小写处理,内部函数，不再导出}
\usage{
decimal2Hex_color(value, format = "upper")
}
\arguments{
\item{value}{填写数值}

\item{format}{返回结果的格式，提供upper及lower等2个选项}
}
\value{
返回值为字符中
}
\description{
将十进制的颜色数值转化为16进制，提供大小写处理,内部函数，不再导出
}
\examples{
 decimal2Hex_color(200,'upper');
}
