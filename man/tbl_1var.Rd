\name{tbl_1var}
\alias{tbl_1var}
\title{Summarize a single variable by the levels of another variable. Default output is LaTeX table. Sample size also given.}
\usage{
  tbl_1var(group_by, value, data_arg, func = mean,
    digits = 2, caption = NULL, display = "latex")
}
\arguments{
  \item{group_by}{name as a string of variable that is used
  like SQL group by.}

  \item{value}{name, as a string, of variable you want to
  summarize.}

  \item{data_arg}{a data frame containing value and
  group_by.}

  \item{func}{(optional, defaults to \code{\link{mean}})
  function to use as summarize by.}

  \item{digits}{(optional) number of digits to print out in
  LaTeX table.}

  \item{caption}{(optional) caption for the LaTeX table.}

  \item{display}{(option) change to anything but "latex" to
  return a standard R table.}
}
\description{
  Example situation: how does average GPA (value) vary by
  Degree (group_by)?
}
\examples{
DF <- data.frame(x = rnorm(100), grp = c(rep("A", 50), rep("B", 50)))
 tbl_1var("grp", "x", DF, display = FALSE)
}

