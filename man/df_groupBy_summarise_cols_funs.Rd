% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-df_summaise_groupLevel.R
\name{df_groupBy_summarise_cols_funs}
\alias{df_groupBy_summarise_cols_funs}
\title{将数据框进行分组计算}
\usage{
df_groupBy_summarise_cols_funs(
  data = mtcars,
  gp_fieldName = c("cyl", "disp"),
  gp_fieldCaption = NULL,
  summ_fieldName = c("mpg", "cyl", "disp"),
  funName = c("mean", "sum", "length"),
  summ_fieldCaption = letters[1:3]
)
}
\arguments{
\item{data}{数据框}

\item{gp_fieldName}{分组变量字段}

\item{gp_fieldCaption}{分组变量标题}

\item{summ_fieldName}{聚合变量字段}

\item{funName}{函数名称}

\item{summ_fieldCaption}{聚会字段标题}
}
\value{
返回数据框
}
\description{
将数据框进行分组计算
}
\examples{
df_groupBy_summarise_cols_funs();
df_groupBy_summarise_cols_funs(mtcars,
gp_fieldName = c('mpg','cyl','disp'),
gp_fieldCaption = c('gp1','gp2','gp3'),
funName = c('sum','sum','sum'),
summ_fieldCaption = c('sum1','sum2','sum3'));
}
