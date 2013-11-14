#' Installs packages, if necessary, and loads them.
#' 
#'  Nice to reproducibility when sharing files which depend on packages with colleagues.
#'  @param model A glmnet object.
#'  @param l_min An optimal value of lambda, typically found through cross validation (e.g. \code{lambda.min}, \code{lambda.1se}, or some other value of lambda found through \code{\link{cv.glmnet}}).
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @import stringr
#'  @importFrom plyr ddply
#'  @importFrom reshape2 melt
#'  @examples
#'  require(glmnet)
#'  require(Matrix)
#'  require(plyr)
#'  require(stringr)
#'  require(reshape2)
#'  set.seed(1010)
#'  n=1000;p=100
#'  nzc=trunc(p/10)
#'  x=matrix(rnorm(n*p),n,p)
#'  beta=rnorm(nzc)
#'  fx= x[,seq(nzc)] %*% beta
#'  eps=rnorm(n)*5
#'  y=drop(fx+eps)
#'  px=exp(fx)
#'  px=px/(1+px)
#'  ly=rbinom(n=length(px),prob=px,size=1)
#'  set.seed(1011)
#'  cv=cv.glmnet(x,y)
#'  lambda_min <- cv$lambda.min
#'  cf <- data.frame(Coefficient = row.names(as.matrix(coef(cv$glmnet.fit))),
#'      Estimate = as.vector(coef(cv$glmnet.fit, s = lambda_min)),
#'      row.names = NULL)
#'  cf <- cf[cf$Estimate != 0,]
#'  cf <- merge(cf, coef_order(cv$glmnet.fit, lambda_min), by = c("Coefficient"), all.x = TRUE)
#'  cf
coef_order <- function(model, l_min){
    all_coefs <- as.matrix(predict(model, type="coef"))[,model$lambda >= l_min]
    all_coefs <- as.data.frame(all_coefs)
    all_coefs$Coefficient <- row.names(all_coefs)
    long_coefs <- melt(all_coefs, id.vars = c("Coefficient"))
    if(length(unique(long_coefs$variable)) <= 1) long_coefs$variable <- "s0"
    coef_order <- long_coefs[long_coefs$value != 0,]
    coef_order$iteration <- as.numeric(str_replace_all(coef_order$variable, "s", ""))
    coef_order <- ddply(coef_order[,c("Coefficient", "iteration")], c("Coefficient"), summarise, First_Iteration = min(iteration))
    rval <- coef_order[order(coef_order$First_Iteration),]
    return(rval)
}