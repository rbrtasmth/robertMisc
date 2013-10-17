#' Signed Log
#' 
#' Returns log of x to base e (default). If x is in [-1, 1] then returns zero. Accepts negative values of x.
#' @param x a number to find log of
#' @param base the base to use for log (default is e)
#' @export
#' @examples
#' sign_log(.5)
#' sign_log(2)
#' sign_log(-2)
sign_log = function(x, base = exp(1)) {
    ifelse(abs(x) <= 1, 0, sign(x)*log(abs(x), base))
}