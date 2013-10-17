\name{xtable_wrap}
\alias{xtable_wrap}
\title{Wrapper function to take just a simple data frame and put it in latex format with my preferred defaults.}
\usage{
  xtable_wrap(xtbl, digits = 2, caption = NULL,
    include_rownames = FALSE, align = NULL, ...)
}
\arguments{
  \item{xtbl}{a table to put in a LaTeX format}

  \item{digits}{Number of digits, passed to
  \code{\link{xtable}}'s \code{digits}}

  \item{caption}{A caption, passed to
  \code{\link{xtable}}'s \code{caption}}

  \item{include_rownames}{Include row names? Defaults to
  \code{FALSE}.}

  \item{align}{Column alignment, passed to
  \code{\link{xtable}}'s \code{align}}

  \item{...}{Additional arguments passed to
  \code{\link{print.xtable}}}
}
\description{
  This wrapper function also includes a few more
  sanitization functions.
}
\examples{
require(xtable)
 xtable_wrap(data.frame(x = 1))
 xtable_wrap(data.frame(x = 1.5), digits = 0)
 xtable_wrap(data.frame(x = 1), caption = "Hello x")
 xtable_wrap(data.frame(x = "a", y = 0), align = c("NULL", "l", "c"))
}

