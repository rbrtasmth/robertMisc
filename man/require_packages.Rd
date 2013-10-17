\name{require_packages}
\alias{require_packages}
\title{Installs packages, if necessary, and loads them.}
\usage{
  require_packages(...)
}
\arguments{
  \item{...}{Name of packages. Can be given as characters
  strings or sans-quotes.}
}
\description{
  Nice to reproducibility when sharing files which depend
  on packages with colleagues.
}
\examples{
require_packages(knitr, formatR)

 ## Not run:
 #  Will result in error
 \dontrun{require_packages(knitr, formatR, not_a_real_package)}
 ## End(**Not run**)

 ### OTHER INPUT FORMATS:  ###
require_packages(c("knitr", "formatR"))
require_packages("knitr", "formatR")
}

