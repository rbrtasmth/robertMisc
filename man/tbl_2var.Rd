\name{tbl_2var}
\alias{tbl_2var}
\title{Similar to \code{\link{tbl_1var}}), summarize two variables into a table. Default output is LaTeX table.}
\usage{
  tbl_2var(var1, var2, value, data_arg, func = mean,
    label = NA, digits = 2, scale = NULL, caption = NULL,
    display = "latex")
}
\arguments{
  \item{var1}{name, as a string, of groups to summarize
  rows of table by.}

  \item{var2}{name, as a string, of groups to summarize
  columns of table by.}

  \item{value}{name, as a string, of variable to
  summarize.}

  \item{data_arg}{a data.frame containing the variables
  var1, var2, and value.}

  \item{func}{(optional) function to summarize by. Defaults
  to mean.}

  \item{label}{(optional) label to add to LaTeX table.}

  \item{digits}{(optional) number of digits to display in
  LaTeX table. Defaults to 2.}

  \item{scale}{(optinoal) number to scale the size of the
  LaTeX table.}

  \item{caption}{(optional) caption to LaTeX table.}

  \item{display}{(optional) defaults to "latex", change to
  anything else to return standard R table.}
}
\description{
  Create pretty LaTeX tables, yay!
}
\examples{
DF <- data.frame(x = rnorm(100),
grp1 = c(rep("A", 50),rep("B", 50)),
grp2 = rep(c(rep("C", 25), rep("D", 25)), 2))
tbl_2var("grp1", "grp2", "x", DF, display = FALSE)
}

