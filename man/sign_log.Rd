\name{sign_log}
\alias{sign_log}
\title{Signed Log}
\usage{
  sign_log(x, base = exp(1))
}
\arguments{
  \item{x}{a number to find log of}

  \item{base}{the base to use for log (default is e)}
}
\description{
  Returns log of x to base e (default). If x is in [-1, 1]
  then returns zero. Accepts negative values of x.
}
\examples{
sign_log(.5)
sign_log(2)
sign_log(-2)
}

