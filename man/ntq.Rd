\name{ntq}
\alias{ntq}
\title{Numeric to quantiles (bin a variable)}
\usage{
  ntq(var_name, quants = 10, data_arg, scale = 1,
    digits = 2, pretty = TRUE)
}
\arguments{
  \item{var_name}{name of variable to bin (passed as a
  string)}

  \item{quants}{number of quantiles to use}

  \item{data_arg}{data.frame containing var_name}
}
\description{
  Use scale to change units (e.g. scale = 365 can convert
  bin labels from days to years) Change pretty to FALSE if
  you want bin labels to just be 1, 2, ..., n
}

