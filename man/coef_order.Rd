\name{coef_order}
\alias{coef_order}
\title{Installs packages, if necessary, and loads them.}
\usage{
  coef_order(model, l_min)
}
\arguments{
  \item{model}{A glmnet object.}

  \item{l_min}{An optimal value of lambda, typically found
  through cross validation (e.g. \code{lambda.min},
  \code{lambda.1se}, or some other value of lambda found
  through \code{\link{cv.glmnet}}).}
}
\description{
  Nice to reproducibility when sharing files which depend
  on packages with colleagues.
}
\examples{
require(glmnet)
 require(Matrix)
 require(plyr)
 require(stringr)
 require(reshape2)
 set.seed(1010)
 n=1000;p=100
 nzc=trunc(p/10)
 x=matrix(rnorm(n*p),n,p)
 beta=rnorm(nzc)
 fx= x[,seq(nzc)] \%*\% beta
 eps=rnorm(n)*5
 y=drop(fx+eps)
 px=exp(fx)
 px=px/(1+px)
 ly=rbinom(n=length(px),prob=px,size=1)
 set.seed(1011)
 cv=cv.glmnet(x,y)
 lambda_min <- cv$lambda.min
 cf <- data.frame(Coefficient = row.names(as.matrix(coef(cv$glmnet.fit))),
     Estimate = as.vector(coef(cv$glmnet.fit, s = lambda_min)),
     row.names = NULL)
 cf <- cf[cf$Estimate != 0,]
 cf <- merge(cf, coef_order(cv$glmnet.fit, lambda_min), by = c("Coefficient"), all.x = TRUE)
 cf
}

