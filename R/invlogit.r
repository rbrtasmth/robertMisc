#' Inverse Logit
#'
#' Returns the inverse logit of a number.
#' @param z a number
#' @export
#' @examples
#' invlogit(.5)
invlogit <- function(z) 1/(1+exp(-z))