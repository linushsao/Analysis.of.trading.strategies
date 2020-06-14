#' A MISC Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

na.filter <- function(x) {return(x[complete.cases(x),])}
